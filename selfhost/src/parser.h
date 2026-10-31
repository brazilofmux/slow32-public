/* parser.h -- Recursive-descent parser for stage04 compiler
 *
 * Phase 2: types, pointers, strings, globals, local arrays.
 * Builds a typed AST from the Ragel lexer token stream.
 * Compiled by stage03 s32-cc.
 */

/* Forward declarations (needed for host GCC compilation of cross-compiler) */
static int find_typedef(char *name);
static int find_local(char *name);
static Node *parse_unary(void);
static int parse_string_literal(void);
static int parse_const_int(void);
static int parse_const_unary(void);
static int pc_hi_of(int lo);
static int ps_rowptr_cty(int elem_ty, int cols);
static void ps_local_rows_cty(int li);
static void next(void);
static void expect(int tok);
static void skip_gnu_attributes(void);

static int is_gnu_qual_ident(void) {
    if (lex_tok != TK_IDENT) return 0;
    if (strcmp(lex_str, "__restrict") == 0) return 1;
    if (strcmp(lex_str, "__restrict__") == 0) return 1;
    if (strcmp(lex_str, "__signed") == 0) return 1;
    if (strcmp(lex_str, "__signed__") == 0) return 1;
    if (strcmp(lex_str, "__const") == 0) return 1;
    if (strcmp(lex_str, "__const__") == 0) return 1;
    if (strcmp(lex_str, "__volatile") == 0) return 1;
    if (strcmp(lex_str, "__volatile__") == 0) return 1;
    return 0;
}

static int is_gnu_attr_ident(void) {
    if (lex_tok != TK_IDENT) return 0;
    if (strcmp(lex_str, "__attribute__") == 0) return 1;
    return 0;
}

static int is_gnu_asm_ident(void) {
    if (lex_tok != TK_IDENT) return 0;
    if (strcmp(lex_str, "__asm__") == 0) return 1;
    if (strcmp(lex_str, "__asm") == 0) return 1;
    return 0;
}

static int is_gnu_extension_ident(void) {
    if (lex_tok != TK_IDENT) return 0;
    if (strcmp(lex_str, "__extension__") == 0) return 1;
    return 0;
}

static int is_c11_atomic_ident(void) {
    if (lex_tok != TK_IDENT) return 0;
    if (strcmp(lex_str, "_Atomic") == 0) return 1;
    return 0;
}

/* Function specifiers and storage words taken and set aside: GNU's
 * inline spellings; C11's _Noreturn (6.7.4: a promise, nothing to
 * emit); and _Thread_local (6.7.1) -- a program here has one thread, so
 * thread storage duration is static storage duration. */
static int is_gnu_inline_ident(void) {
    if (lex_tok != TK_IDENT) return 0;
    if (strcmp(lex_str, "__inline") == 0) return 1;
    if (strcmp(lex_str, "__inline__") == 0) return 1;
    if (strcmp(lex_str, "_Noreturn") == 0) return 1;
    if (strcmp(lex_str, "_Thread_local") == 0) return 1;
    if (strcmp(lex_str, "_Alignas") == 0) return 1;    /* read by ps_read_alignas */
    return 0;
}

static int is_gnu_typeof_ident(void) {
    if (lex_tok != TK_IDENT) return 0;
    if (strcmp(lex_str, "typeof") == 0) return 1;
    if (strcmp(lex_str, "__typeof") == 0) return 1;
    if (strcmp(lex_str, "__typeof__") == 0) return 1;
    return 0;
}

static void skip_balanced_parens(void) {
    int depth;

    if (lex_tok != TK_LPAREN) return;
    depth = 0;
    while (lex_tok != TK_EOF) {
        if (lex_tok == TK_LPAREN) depth = depth + 1;
        else if (lex_tok == TK_RPAREN) {
            depth = depth - 1;
            next();
            if (depth <= 0) break;
            continue;
        }
        next();
    }
}

static int gnu_float_ident_ty(void) {
    if (lex_tok != TK_IDENT) return -1;
    if (strcmp(lex_str, "_Float32") == 0) return TY_FLOAT;
    if (strcmp(lex_str, "_Float32x") == 0) return TY_DOUBLE;
    if (strcmp(lex_str, "_Float64") == 0) return TY_DOUBLE;
    if (strcmp(lex_str, "_Float64x") == 0) return TY_DOUBLE;
    if (strcmp(lex_str, "_Float128") == 0) return TY_DOUBLE;
    if (strcmp(lex_str, "__float128") == 0) return TY_DOUBLE;
    return -1;
}

/* parse_type saw volatile (or __volatile__): before the first star
 * (ps_type_volb, the base object -- or the pointee, when stars follow)
 * or after one (ps_type_volp, the pointer object itself).  Declarations
 * record both and read them against the declared type (ps_var_vol);
 * casts carry the base bit onto a pointer (docs/plans/memory-model.md,
 * step 2a).  Declared here, ahead of skip_decl_qualifiers. */
static int ps_type_volb;
static int ps_type_volp;

/* const, tracked only far enough to say whether a file-scope object is
 * itself const, so that stage08 can place it in .rodata (selfhost
 * ISSUES-99): ps_type_constb is const before the first star (the base
 * object, or the pointee when stars follow), ps_type_nstar the stars,
 * ps_type_constp const after the LAST star (the pointer object itself).
 * ps_skipq_const is const seen by skip_decl_qualifiers since its caller
 * cleared it.  A miss leaves an object writable, as every object was
 * before; only a false "const" could break a program. */
static int ps_type_constb;
/* The C type parse_type read (docs/TYPES.md): the base, its qualifiers,
 * and a ct_ptr per star.  Declarators beyond the stars -- arrays,
 * functions -- are the declaration's to add. */
static int ps_type_cty;
static int ps_type_nostar_cty;   /* ... and its base, before the stars (ps_type_nostar's) */
static int ps_skipq_cq;          /* CQ_ bits skip_decl_qualifiers consumed */
static int ps_type_nstar;

/* The C type to record for an object whose machine type is ty: the one
 * parse_type built, when it agrees -- else one rebuilt from ty, which
 * loses what the int encoding cannot say.  S12CC_CTY_CHECK=1 counts the
 * rebuilds by machine type on stderr at exit: where a declaration path
 * still loses the C type (docs/TYPES.md, S1). */
static int ps_cty_check = -1;
static int ps_cty_fallbacks;
static int ps_global_cty(int ty, int size_bytes);
static void ps_global_alignas(int idx, int ty, int size_bytes);
/* A function-pointer member's C type, taken when its (*name) is read and
 * consumed when the member is registered (its parameter list, between,
 * runs parse_type again). */
static int ps_pending_member_cty;
static int ps_init_cty;           /* an initializer's target C type (ps_init_check) */
static int ps_desig_cty;          /* ... and a designator's, as it walks */
static void ps_init_bool_at(int gidx, int rel_off);
static void sema_const_conv(Node *at, int lt, Node *r, int bits);    /* sema.h */
/* inline in the file-scope declaration being read (ps_storage_next):
 * an object may not be inline, nor main (6.7.4p3, 5.1.2.2.1) */
static int ps_top_inline;
static int ps_pend_alignas;      /* _Alignas read before parse_type ... */
static int ps_type_alignas;      /* ... and the declaration's, once parse_type has run */
static void ps_read_alignas(void);
static void ps_member_static_assert(void);
static int ps_local_alignas(int elem_ty);
static int ps_fn_align;           /* the strongest _Alignas on a local of this function */
/* Variable length arrays: the VLA objects of the function, each with
 * its enclosing one (the innermost in scope when it was declared) and
 * the hidden local that holds sp from before it was allocated --
 * setting sp back to that frees it and everything since.  ps_vd_top is
 * the innermost in scope, -1 none. */
#define PS_MAX_VD 1024
static int   ps_vd_parent[PS_MAX_VD];
static Node *ps_vd_save[PS_MAX_VD];
static int   ps_nvd;
static int   ps_vd_top;
static int   ps_fn_vm;           /* the function declares a variably modified type */
static void sema_expr(Node *n);  /* sema.h */
static long long sema_cvalof(Node *c);
static void ps_lex_mark(void);
static void ps_lex_commit(void);
static void ps_lex_rewind(void);
static void ps_typedef_check(void);
static int ps_vla_ahead(void);
static Node *ps_local_vm_ptr(char *nm, int ty, int nstars, int is_static);
static int ps_decl_inner;          /* a typedef'd array's own dimension (parse_local_array_dims) */
static void ps_align_stack(int a);
/* static in the file-scope declaration being read: one that follows a
 * declaration with external linkage is an error (6.2.2p7) */
static int ps_top_static;
/* The scope a local is declared in: a block, or a for statement's
 * clause-1.  Redeclaring an identifier with no linkage in one scope is
 * an error (6.7p3); in an inner one it is shadowing. */
static int ps_scope_id;
static int ps_scope_next;
static int ps_block_depth;      /* block nesting: 0 at file scope */
static int sema_compat(int a, int b);

/* The stars of a (**name)(...) declarator, consumed and counted. */
static int ps_fnptr_nstar = 1;
static void ps_skip_fnptr_stars(void) {
    ps_fnptr_nstar = 0;
    while (lex_tok == TK_STAR) { next(); ps_fnptr_nstar = ps_fnptr_nstar + 1; }
}

/* The type a (**name)(...) declarator makes of the type read before it:
 * as many pointers as it had stars (ps_fnptr_nstar), to a function; the
 * parameters are not recorded here.  It was always one pointer, so
 * void (**aExt)(void) made aExt[0] a function, sizeof 1. */
static int ps_fnptr_cty(int ret_cty) {
    int t;
    int k;
    t = ct_func(ret_cty, 0, -1, 0);
    k = ps_fnptr_nstar > 0 ? ps_fnptr_nstar : 1;
    while (k > 0) { t = ct_ptr(t); k = k - 1; }
    return t;
}

/* ps_obj_cty without the report: for a type already recorded once. */
static int ps_cty_quiet(int ty) {
    if (ps_type_cty > 0 && ct_mty_agrees(ps_type_cty, ty)) return ps_type_cty;
    return ct_from_mty(ty, 0);
}

static int ps_obj_cty(int ty) {
    if (ps_type_cty > 0 && ct_mty_agrees(ps_type_cty, ty)) return ps_type_cty;
    if (ps_cty_check < 0) ps_cty_check = getenv("S12CC_CTY_CHECK") != 0;
    if (ps_cty_check) {
        ps_cty_fallbacks = ps_cty_fallbacks + 1;
        fdputs("cty-fallback ty=", 2);
        fdputuint(2, (unsigned)ty);
        fdputs(" parsed=", 2);
        fdputuint(2, (unsigned)(ps_type_cty > 0 ? ct_mty(ps_type_cty) : 0));
        fdputs(" at line ", 2);
        fdputuint(2, (unsigned)lex_line);
        fdputs(" before ", 2);
        if (lex_tok == TK_IDENT) fdputs(lex_str, 2);
        else fdputs("(punct)", 2);
        fdputs("\n", 2);
    }
    return ct_from_mty(ty, 0);
}
static int ps_type_constp;
static int ps_skipq_const;

/* The bits of a declaration read against its shape: an array's elements
 * are volatile when the base was (and so is what its decayed pointer
 * reaches); a pointer object is volatile when the qualifier followed a
 * star, and points to volatile when it preceded one; anything else is
 * volatile when the base was. */
static void ps_var_vol(Node *n, int ty, int is_arr, int volb, int volp) {
    if (is_arr) { n->is_vol = volb; n->is_volptr = volb; }
    else if (ty_is_ptr(ty)) { n->is_vol = volp; n->is_volptr = volb; }
    else { n->is_vol = volb; n->is_volptr = 0; }
}

static int ps_tok_volatile(void) {
    if (lex_tok == TK_VOLATILE) return 1;
    return lex_tok == TK_IDENT && strncmp(lex_str, "__volatile", 10) == 0;
}

/* --- Arrays of more than two dimensions (ast.h ast_sub_cols) --- */

/* The stride entry of the array declarator just read, for its caller to
 * record beside the row width (parse_local_array_dims, the global and
 * parameter declarators). */
static int ps_dims_sub;

/* A compound literal's array type (T[N][M]...): the elements its first
 * subscript steps over, 0 for one dimension, and the strides below. */
static int ps_cl_cols;
static int ps_cl_sub;

/* The entry for a subscript stepping over cols elements, the one after
 * it next: shared when it already exists. */
static int ps_sub(int cols, int nxt) {
    int i;
    i = 1;
    while (i < ast_nsubs) {
        if (ast_sub_cols[i] == cols && ast_sub_next[i] == nxt) return i;
        i = i + 1;
    }
    if (ast_nsubs >= AST_MAX_SUBS) {
        p_error("too many array shapes");
        return 0;
    }
    ast_sub_cols[ast_nsubs] = cols;
    ast_sub_next[ast_nsubs] = nxt;
    ast_nsubs = ast_nsubs + 1;
    return ast_nsubs - 1;
}

/* The dimensions after an array's first, dims[0..nd-1]: the elements
 * the first subscript steps over (their product; 0 when there are
 * none), and in *psub the entry for the subscripts below it. */
static int ps_dims_shape(int *dims, int nd, int *psub) {
    int k;
    int prod;
    int sub;
    sub = 0;
    prod = 1;
    k = nd - 1;
    while (k >= 1) {
        prod = prod * dims[k];
        sub = ps_sub(prod, sub);
        k = k - 1;
    }
    *psub = sub;
    if (nd == 0) return 0;
    return prod * dims[0];
}

/* The dimensions after an array's first, the cursor on the `[` of the
 * second (or past the last): each must be sized.  extra, when > 0, is
 * a typedef'd array's own dimension, the innermost.  Returns what
 * ps_dims_shape does. */
static int ps_inner_dims(int extra, int *psub) {
    int dims[16];
    int nd;
    nd = 0;
    while (lex_tok == TK_LBRACK) {
        next();
        if (lex_tok == TK_RBRACK) {
            p_error("array size required without initializer");
            dims[0] = 1;
        } else if (nd < 15) {
            dims[nd] = parse_const_int();
            nd = nd + 1;
        } else {
            p_error("too many array dimensions");
            parse_const_int();
        }
        expect(TK_RBRACK);
    }
    if (extra > 0) {
        dims[nd] = extra;
        nd = nd + 1;
    }
    return ps_dims_shape(dims, nd, psub);
}

/* The C type of an object of cols elements elem shaped by sub: a row of
 * cols for sub 0, else an array of the rows sub describes. */
static int ps_rows_cty(int elem, int cols, int sub) {
    if (sub <= 0) return ct_array(elem, cols);
    return ct_array(ps_rows_cty(elem, ast_sub_cols[sub], ast_sub_next[sub]),
                    cols / ast_sub_cols[sub]);
}

/* The scalars an object of C type c holds, through all its array
 * dimensions (1 for one that is not an array). */
static int ps_cty_scalars(int c) {
    int n;
    n = 1;
    while (c > 0 && ct_is_array(c)) {
        n = n * ct_countof(c);
        c = ct_baseof(c);
    }
    return n;
}

/* Is c an array of arrays, its rows of known size (a multi-dimensional
 * array, by its C type)? */
static int ps_cty_rows(int c) {
    return c > 0 && ct_is_array(c) && ct_is_array(ct_baseof(c)) &&
           ct_countof(ct_baseof(c)) > 0;
}

/* The stride entry of the rows inside an array type x: 0 when its
 * element is not itself an array.  int [3][4] has rows of 4, {4, 0}. */
static int ps_cty_sub(int x) {
    if (!ps_cty_rows(x)) return 0;
    return ps_sub(ps_cty_scalars(ct_baseof(x)), ps_cty_sub(ct_baseof(x)));
}

/* A struct member's C type, from what its registration recorded: an
 * array of its element (a row of arr_cols for a 2-D one), or the
 * object type -- the one parse_type built when it agrees (ps_obj_cty). */
static int ps_member_cty(int mi) {
    int elem;
    int esz;
    int count;
    int t;
    if (!stm_is_arr[mi] && stm_arr_cols[mi] > 0 && ty_is_ptr(stm_type[mi])) {
        /* T (*name)[N]: a pointer to an array of N (its declarator went
         * through the function-pointer path, so the pending C type is a
         * function pointer's) */
        ps_pending_member_cty = 0;
        return ct_ptr(ps_rows_cty(ct_from_mty(ty_deref(stm_type[mi]), 0),
                                  stm_arr_cols[mi], stm_arr_sub[mi]));
    }
    if (ps_pending_member_cty > 0 && !stm_is_arr[mi]) {
        t = ps_pending_member_cty;
        ps_pending_member_cty = 0;
        if (ct_mty_agrees(t, stm_type[mi])) return t;
    }
    ps_pending_member_cty = 0;
    if (!stm_is_arr[mi]) return ps_obj_cty(stm_type[mi]);
    elem = ps_obj_cty(ty_deref(stm_type[mi]));
    esz = ty_size(ty_deref(stm_type[mi]));
    count = (esz > 0 && stm_arr_size[mi] > 0) ? stm_arr_size[mi] / esz : -1;
    if (stm_arr_cols[mi] > 0 && count > 0)
        return ct_array(ps_rows_cty(elem, stm_arr_cols[mi], stm_arr_sub[mi]),
                        count / stm_arr_cols[mi]);
    return ct_array(elem, count);
}

/* The qualifier a qualifier token adds to a C type (0 for an attribute
 * or other GNU qualifier word). */
static int ps_tok_cq(void) {
    if (lex_tok == TK_CONST) return CQ_CONST;
    if (lex_tok == TK_RESTRICT) return CQ_RESTRICT;
    if (ps_tok_volatile()) return CQ_VOLATILE;
    return 0;
}

static void skip_decl_qualifiers(void) {
    while (1) {
        if (lex_tok == TK_IDENT && strcmp(lex_str, "_Alignas") == 0) {
            ps_pend_alignas = ps_type_alignas;
            ps_read_alignas();
            ps_type_alignas = ps_pend_alignas;
            ps_pend_alignas = 0;
            continue;
        }
        if (lex_tok == TK_CONST || lex_tok == TK_VOLATILE ||
            lex_tok == TK_RESTRICT || lex_tok == TK_INLINE ||
            is_gnu_qual_ident() || is_gnu_extension_ident() ||
            is_gnu_inline_ident()) {
            if (ps_tok_volatile()) ps_type_volp = 1;
            if (lex_tok == TK_CONST) ps_skipq_const = 1;
            ps_skipq_cq = ps_skipq_cq | ps_tok_cq();
            next();
            continue;
        }
        if (is_c11_atomic_ident()) {
            next();
            if (lex_tok == TK_LPAREN) skip_balanced_parens();
            continue;
        }
        if (is_gnu_attr_ident()) {
            skip_gnu_attributes();
            continue;
        }
        break;
    }
}

/* A declarator's stars after the first: `*` and their qualifiers, onto
 * machine type ty and C type cty (0: rebuild it from ty).  Leaves the
 * C type in ps_type_cty, the count in ps_type_nstar, and in
 * ps_type_constp whether the last star was const-qualified (what
 * ISSUES-99's .rodata placement reads). */
static int ps_declarator_stars(int ty, int cty) {
    ps_type_cty = cty > 0 ? cty : ct_from_mty(ty, 0);
    ps_type_nstar = 0;
    ps_type_constp = 0;
    while (lex_tok == TK_STAR) {
        ty = ty + TY_PTR;
        next();
        ps_skipq_const = 0;
        ps_skipq_cq = 0;
        skip_decl_qualifiers();
        ps_type_nstar = ps_type_nstar + 1;
        ps_type_constp = ps_skipq_const;
        ps_type_cty = ct_qualify(ct_ptr(ps_type_cty), ps_skipq_cq);
    }
    return ty;
}

/* Grouping parens around a declarator: `char *(name[])` is `char *name[]`,
 * not a function pointer.  After '(' the current token is the identifier;
 * if the next raw byte is '[' or ')', blank the matching ')' so the rest
 * of the declarator parses as ordinary.  Returns 1 if grouped. */
static int ps_try_grouping_paren(void) {
    char *q;
    int depth;
    if (lex_tok != TK_IDENT) return 0;
    q = lex_rp;
    while (*q == 32 || *q == 9 || *q == 10) q = q + 1;
    if (*q != 91 && *q != 41) return 0;          /* '[' or ')' */
    depth = 0;
    while (*q) {
        if (*q == 40 || *q == 91) depth = depth + 1;
        else if (*q == 93) depth = depth - 1;
        else if (*q == 41) {
            if (depth == 0) { *q = 32; return 1; }
            depth = depth - 1;
        }
        q = q + 1;
    }
    return 1;
}

static void skip_gnu_attributes(void) {
    int depth;

    while (is_gnu_attr_ident()) {
        next();
        if (lex_tok != TK_LPAREN) continue;
        depth = 0;
        while (lex_tok != TK_EOF) {
            if (lex_tok == TK_LPAREN) depth = depth + 1;
            else if (lex_tok == TK_RPAREN) {
                depth = depth - 1;
                next();
                if (depth <= 0) break;
                continue;
            }
            next();
        }
    }
}

static void skip_gnu_decl_suffixes(void) {
    int depth;

    while (1) {
        if (is_gnu_attr_ident() || is_gnu_asm_ident()) {
            next();
            if (lex_tok != TK_LPAREN) continue;
            depth = 0;
            while (lex_tok != TK_EOF) {
                if (lex_tok == TK_LPAREN) depth = depth + 1;
                else if (lex_tok == TK_RPAREN) {
                    depth = depth - 1;
                    next();
                    if (depth <= 0) break;
                    continue;
                }
                next();
            }
            continue;
        }
        break;
    }
}

static int target_long_ty(void) {
    if (ty_ptr_size == 8) return TY_LLONG;
    return TY_INT;
}

/* A sizeof or offsetof result: size_t, unsigned long (6.5.3.4p5,
 * 7.19p2).  It was int, so (n + 1) * sizeof(double) sign-extended into
 * a long long and -sizeof(struct s) was negative. */
static Node *nd_size(int v) {
    return nd_num64(v, 0, target_long_ty() | TY_UNSIGNED);
}

/* --- Shared label counter (used by both parser and codegen) --- */
static int cg_lbl;    /* label counter (monotonically increasing) */

static int cg_label(void) {
    int l;
    l = cg_lbl;
    cg_lbl = cg_lbl + 1;
    return l;
}

/* --- Parser state --- */
#define P_MAX_LOCALS  1024   /* was 128 */
#define P_MAX_GLOBALS 32768   /* was 8192 */
#define PS_MAX_CONSTS 16384   /* was 2048 */

static int   ps_lscope[P_MAX_LOCALS];  /* the scope it was declared in (ps_scope_id) */
static char *ps_lname[P_MAX_LOCALS];  /* local var names */
static int   ps_loff[P_MAX_LOCALS];   /* local var offsets from fp */
static int   ps_ltype[P_MAX_LOCALS];  /* local var types */
static int   ps_lcty[P_MAX_LOCALS];   /* ... their C types (ctype.h) */
static int   ps_larr[P_MAX_LOCALS];   /* 1 if array (addr, no load) */
static int   ps_lcols[P_MAX_LOCALS];  /* 2D arrays: last-dim count (0 = 1D) */
static int   ps_lsub[P_MAX_LOCALS];   /* past 2 dimensions: ast_sub_cols entry */
static int   ps_lsize[P_MAX_LOCALS];  /* total byte size (arrays: elem_sz*count) */
static int   ps_lstatic[P_MAX_LOCALS]; /* 1 = static local, 0 = normal */
static char *ps_lsname[P_MAX_LOCALS];  /* mangled name (static locals only) */
static int   ps_lreg[P_MAX_LOCALS];    /* 1 = declared register: no & (6.5.3.2p1) */
static int   ps_lvolb[P_MAX_LOCALS];   /* volatile before the stars, at declaration */
static int   ps_lvolp[P_MAX_LOCALS];   /* ... and after them */
static int   ps_lfpbase[P_MAX_LOCALS]; /* function-pointer local: base into ps_fptypes, -1 = none */
static int   ps_lfpn[P_MAX_LOCALS];
static int   ps_nlocals;
static int   ps_stack;                /* current stack allocation */
static int   ps_stack_max;            /* high-water mark (fn->locals_size) */
static int   ps_lslot[P_MAX_LOCALS];  /* per-declaration id; never reused */
static int   ps_fn_setjmp;            /* the function being parsed has called setjmp */
static int   ps_slot_gen;
static int   ps_li_slot;              /* slot_id of the local being initialized */
static int   ps_nparams;              /* params in current func */
static int   ps_is_varargs;           /* 1 if current func has ... */
static int   ps_struct_ret;           /* 1 if current func returns struct via hidden ptr */
static int   ps_retptr_off;           /* stack offset of hidden __retptr param */
static int   ps_comp_lit_id;          /* unique hidden locals for compound literals */

static char *ps_gname[P_MAX_GLOBALS]; /* global var names */
static int   ps_gvolb[P_MAX_GLOBALS]; /* volatile before the stars, at declaration */
static int   ps_gvolp[P_MAX_GLOBALS]; /* ... and after them */
static int   ps_galign[P_MAX_GLOBALS]; /* _Alignas, in bytes: more than a word is .align'd (0 = none) */
static int   ps_gconst[P_MAX_GLOBALS]; /* the object itself is const: .rodata (ISSUES-99) */
/* The const-ness of the declarator about to be added, set by
 * parse_top_decl just before its add_*_global call and consumed (reset)
 * by that call, so it cannot reach a later declaration. */
static int   ps_decl_objconst;
static int   ps_gtype[P_MAX_GLOBALS]; /* global var types */
static int   ps_gcty[P_MAX_GLOBALS];  /* ... their C types (ctype.h) */
static int   ps_gsize[P_MAX_GLOBALS]; /* size in bytes (0=scalar, >0=array) */
static int   ps_ginit[P_MAX_GLOBALS]; /* initial value for scalars */
static int   ps_ginit_hi[P_MAX_GLOBALS]; /* hi word for 64-bit global initializers */
static int   ps_gstr[P_MAX_GLOBALS];  /* string init: pool index, -1 if none */
static int   ps_glocal[P_MAX_GLOBALS]; /* 1 = static local (suppress .global) */
static int   ps_gextern[P_MAX_GLOBALS]; /* 1 = declaration only, no storage */
static int   ps_gcols[P_MAX_GLOBALS];   /* 2D arrays: last-dim count (0 = 1D) */
static int   ps_gsub[P_MAX_GLOBALS];    /* past 2 dimensions: ast_sub_cols entry */
static int   ps_ghasinit[P_MAX_GLOBALS]; /* 1 = completed initializer (incl. = 0) */
static int   ps_gfpbase[P_MAX_GLOBALS]; /* function-pointer global: base into ps_fptypes */
static int   ps_gfpn[P_MAX_GLOBALS];
static int   ps_nglobals;

/* Static local variable state */
static char *ps_cur_func;             /* current function name, NULL outside */
static char  ps_sl_buf[256];          /* scratch buffer for name mangling */
static int   ps_sl_count;             /* global static-local counter */

/* Array/struct initializer bytes for globals */
#define PS_MAX_INIT_POOL 4194304   /* was 262144 */  /* doom's tables.c initializes ~80KB of LUTs */
static unsigned char ps_ginit_pool[PS_MAX_INIT_POOL];
static int ps_ginit_start[P_MAX_GLOBALS]; /* -1 = no init bytes */
static int ps_ginit_count[P_MAX_GLOBALS]; /* byte count */
static int ps_ginit_pool_len;

#define PS_MAX_INIT_RELOCS 65536   /* was 8192 */
#define GIRELOC_STRING 0
#define GIRELOC_GLOBAL 1
#define GIRELOC_SYMBOL 2
static int ps_girel_start[P_MAX_GLOBALS];
static int ps_girel_count[P_MAX_GLOBALS];
static int ps_girel_off[PS_MAX_INIT_RELOCS];
static int ps_girel_kind[PS_MAX_INIT_RELOCS];
static int ps_girel_idx[PS_MAX_INIT_RELOCS];
static int ps_girel_size[PS_MAX_INIT_RELOCS];
static int   ps_girel_add[PS_MAX_INIT_RELOCS]; /* byte addend for GIRELOC_SYMBOL (&arr[k]) */
static char *ps_girel_name[PS_MAX_INIT_RELOCS];
static int ps_ngirelocs;

/* goto/label table (per-function, reset at each function) */
#define P_MAX_LABELS 4096   /* was 512 */
static char *ps_lblname[P_MAX_LABELS];
static int   ps_lbldef[P_MAX_LABELS];    /* 1 once `name:` has been read (goto alone does not) */
static int   ps_lblvtop[P_MAX_LABELS];  /* the VLA object innermost in scope at it (ps_vd_top) */
static int   ps_lblid[P_MAX_LABELS];
static int   ps_nlabels;

/* Enum constant table */
static char *ps_cname[PS_MAX_CONSTS];
static int   ps_cval[PS_MAX_CONSTS];
static int   ps_nconsts;

/* Typedef table */
#define PS_MAX_TYPEDEFS 4096   /* was 1024 */
static int   ps_tddepth[PS_MAX_TYPEDEFS];  /* block depth declared at; -1 = the compiler's own */
static char *ps_tdname[PS_MAX_TYPEDEFS];
static int   ps_tdtype[PS_MAX_TYPEDEFS];
static int   ps_tdcty[PS_MAX_TYPEDEFS];   /* the typedef's C type, declarator included */
static int   ps_tdarr[PS_MAX_TYPEDEFS];  /* array typedefs: element count (0 = scalar) */
static int   ps_tdfpbase[PS_MAX_TYPEDEFS]; /* function-pointer typedef: ps_fptypes base */
static int   ps_tdfpn[PS_MAX_TYPEDEFS];
static int   ps_ntypedefs;
static int   ps_in_body;    /* parsing a function's body: its locals are in scope (is_type) */
/* Set by parse_type: element count when the type came from an array
 * typedef (typedef byte sha1_digest_t[20]); a declaration of that
 * type must create a real array, not the decayed pointer (SHA1_Final
 * wrote its digest through an uninitialized 4-byte 'pointer'). */
static int   ps_type_arrcount;
static int   ps_type_nostar;    /* parse_type's answer before its trailing stars:
                                 * what a declarator after a comma starts from
                                 * (`str a, b;` with str a typedef'd pointer) */
static int   ps_type_fpbase;  /* function-pointer typedef: signature of this parse_type */
static int   ps_type_fpn;

/* Function signature table (return type, variadic flag, and declared
 * parameter types).  Param types drive implicit argument conversion at
 * call sites (issue #6: `mul(k, 7)` with long long params passed the
 * int 7 as ONE register slot, leaving the pair's high word stale). */
#define PS_MAX_FUNCS 16384   /* was 4096 */
#define PS_MAX_FPARAM 65536   /* was 16384 */
static int   ps_fdefined[PS_MAX_FUNCS]; /* 1 once its body has been read */
static char *ps_fname[PS_MAX_FUNCS];
static int   ps_ftype[PS_MAX_FUNCS];
static int   ps_fcty[PS_MAX_FUNCS];   /* the function's C type (ctype.h), docs/TYPES.md */
static int   ps_fvar[PS_MAX_FUNCS];   /* 1 = variadic (affects f64 arg ABI) */
static int   ps_fpbase[PS_MAX_FUNCS]; /* base into ps_fptypes, -1 = unknown */
static int   ps_fpn[PS_MAX_FUNCS];    /* declared param count */
static int   ps_fretfpbase[PS_MAX_FUNCS]; /* return type is a function pointer */
static int   ps_fretfpn[PS_MAX_FUNCS];
static int   ps_fptypes[PS_MAX_FPARAM];
static int   ps_fpctypes[PS_MAX_FPARAM]; /* ... their C types, unqualified (6.7.6.3p15) */
static int   ps_fpret[PS_MAX_FPARAM];  /* return type of the fnptr signature at this base; 0 = TY_INT */
static int   ps_fpvar[PS_MAX_FPARAM];  /* 1 = the signature at this base ends in `...` */
static int   ps_nfptypes;
static int   ps_nfuncs;
static int   ps_pending_retfpbase = -1;
static int   ps_pending_retfpn;

/* Forward declarations */
static Node *parse_expr(void);
static Node *parse_stmt(void);
static Node *parse_assign(void);
static Node *parse_postfix(void);
static Node *parse_postfix_from(Node *n);
static Node *parse_gnu_asm_stmt(void);
static Node *parse_block(void);
/* ps_parse_fp_params sits above these and calls them (GitHub issue 71). */
static int is_type(void);
static Node *parse_primary(void);
static int parse_type(void);
static int ps_fp_store(int *ptys, int *pctys, int np);
static int ps_enum_value(void);
static int ps_array_size(void);

/* --- Utilities --- */

static void p_error(char *msg);
static void ps_lex_warning(void);
static void next(void);

/* The _Pragma operator (6.10.9): _Pragma ( string-literal ), the string
 * destringized and taken as a #pragma -- only `once` does anything, as
 * for the directive; the rest are skipped.  The cursor is on _Pragma.
 * A macro may produce it: DO_PRAGMA(x) _Pragma(#x).  It was an
 * undeclared identifier. */
static void ps_pragma_operator(void) {
    char *sp;
    int n;
    next();
    if (lex_tok != TK_LPAREN) p_error("_Pragma takes a parenthesized string literal");
    next();
    if (lex_tok != TK_STRING) p_error("_Pragma takes a parenthesized string literal");
    sp = lex_strpool + lex_str_off[lex_val];
    n = lex_str_len[lex_val];
    while (n > 0 && (*sp == 32 || *sp == 9)) { sp = sp + 1; n = n - 1; }
    if (n >= 4 && strncmp(sp, "once", 4) == 0 && (n == 4 || sp[4] == 32) && pp_nonce < PP_MAX_ONCE) {
        pp_once[pp_nonce] = strdup(pp_curfile);
        pp_nonce = pp_nonce + 1;
    }
    next();
    if (lex_tok != TK_RPAREN) p_error("_Pragma takes a parenthesized string literal");
}

static void next(void) {
    int di;
    int bol;
    while (1) {
        if (pp_skip) {
            pp_fast_skip_to_directive();
            lex_bol = 1;
            if (lex_pos >= lex_len) {
                pp_error("unterminated conditional directive");
                lex_tok = TK_EOF;
                pp_sync();
                return;
            }
            pp_sync();
        }
        lex_next();
        bol = lex_bol;
        lex_bol = 0;
        if (lex_err) p_error(lex_err);
        if (lex_warn) ps_lex_warning();
        /* A backslash-newline outside a directive or literal is a line
         * splice (translation phase 2), which the scanner has no rule for
         * and stops on as if at end of input: step over it and go on.
         * SQLite's charFunc has one after a closing brace. */
        if (lex_tok == TK_EOF && lex_pos < lex_len && lex_src[lex_pos] == 92) {
            if (lex_src[lex_pos + 1] == 10) { lex_pos = lex_pos + 2; lex_line = lex_line + 1; pp_sync(); continue; }
            if (lex_src[lex_pos + 1] == 13 && lex_src[lex_pos + 2] == 10) { lex_pos = lex_pos + 3; lex_line = lex_line + 1; pp_sync(); continue; }
        }
        if (lex_tok == TK_EOF && lex_pos >= lex_len && pp_dep > 0)
            pp_error("unterminated conditional directive");
        /* A # begins a directive only as a line's first token: one a
         * macro produced is a stray token (6.10p8: EMPTY # define Z). */
        if (lex_tok == TK_HASH && bol) { pp_directive(); continue; }
        if (pp_skip) { continue; }
        /* offsetof is a keyword only where no macro of that name is
         * defined: arguments are expanded as text (pp_preexpand), and a
         * program's own offsetof must mean one thing in both. */
        if ((lex_tok == TK_IDENT || (lex_tok == TK_OFFSETOF && lex_str[0] == 111)) && !lex_painted) {
            /* Dynamic predefs: values track position / primary source path. */
            if (strcmp(lex_str, "__LINE__") == 0) {
                lex_tok = TK_NUM;
                lex_val = lex_line;
                lex_val_hi = 0;
                lex_val_ll = 0;
                lex_val_l = 0;
                lex_val_u = 0;
                lex_val_hexoct = 0;
                lex_val_macro = 1;
                return;
            }
            if (strcmp(lex_str, "__FILE__") == 0) {
                if (pp_curfile[0] != 0) {
                    pp_emit_string_token(pp_curfile);
                } else {
                    pp_emit_string_token("");
                }
                return;
            }
            if (lex_str[0] == 95 && strcmp(lex_str, "_Pragma") == 0) {
                ps_pragma_operator();
                continue;
            }
            /* keywords (6.4.1) of the complex types, which are not
             * implemented: as names they were taken as identifiers */
            if (lex_str[0] == 95 && (strcmp(lex_str, "_Imaginary") == 0 || strcmp(lex_str, "_Complex") == 0))
                p_error("_Complex and _Imaginary are not supported");
            di = pp_find(lex_str);
            if (di >= 0 && pp_disabled(di, (int)(lex_rp - lex_src) - lex_slen)) di = -1;
            if (di >= 0) {
                if (pp_dnpar[di] >= 0) {
                    if (pp_expand_func(di)) continue;
                    return;  /* no '(' — treat as identifier */
                }
                if (pp_dbody[di] != 0) {
                    pp_expand_obj(di);
                    continue;  /* re-lex expanded text */
                }
                lex_tok = TK_NUM;
                lex_val = pp_dval[di];
                /* #define values are 32-bit int; clear the 64-bit state
                 * so a preceding 64-bit literal's flags don't bleed in.
                 * lex_val_macro keeps `#define N -5` as a signed int:
                 * issue 40's "v < 0 means 2147483648" rule is only for
                 * lexer-originated decimal tokens. */
                lex_val_hi = 0;
                lex_val_ll = 0;
                lex_val_l = 0;
                lex_val_u = 0;
                lex_val_hexoct = 0;
                lex_val_macro = 1;
                return;
            }
        }
        return;
    }
}

/* A lexer warning (lex_warn), where the token is. */
static void ps_lex_warning(void) {
    pp_where();
    fdputs("warning: ", 2);
    fdputs(lex_warn, 2);
    fdputs("\n", 2);
    lex_warn = 0;
}

/* Print the text of the stream line the lexer is on, so a diagnostic
 * carries the offending source and not just a token number.  lex_pos is
 * just past the current token; walk back to the line start. */
static void p_show_line(void) {
    int b;
    int e;
    int n;
    b = lex_pos;
    if (b > lex_len) b = lex_len;
    while (b > 0 && lex_src[b - 1] != 10) b = b - 1;
    e = b;
    n = 0;
    fdputs("  | ", 2);
    while (e < lex_len && lex_src[e] != 10 && n < 200) {
        fdputc(lex_src[e], 2);
        e = e + 1;
        n = n + 1;
    }
    fdputc(10, 2);
}

static void p_error(char *msg) {
    pp_where();
    fdputs("error: ", 2);
    fdputs(msg, 2);
    fdputc(10, 2);
    p_show_line();
    exit(1);
}

/* After a '(' has been consumed: skip to and past the matching ')'.
 * Parameter lists may themselves hold function-pointer parameters,
 * so the depth is counted. */
static void p_skip_to_rparen(void) {
    int depth;
    depth = 1;
    while (lex_tok != TK_EOF) {
        if (lex_tok == TK_LPAREN) depth = depth + 1;
        if (lex_tok == TK_RPAREN) {
            depth = depth - 1;
            if (depth == 0) { next(); return; }
        }
        next();
    }
}


/* Parameter types of a function-pointer declarator, entered just after
 * its '('; consumes through the ')'.  Returns a base into ps_fptypes
 * (ps_fp_store) with the count in *np_out, or -1 when the list is not
 * a prototype.  Only the marshalling class matters to the consumer
 * (sema's argument conversion), so declarators are reduced to base
 * type plus pointer depth.  SQLite's xTruncate(sqlite3_file*,
 * sqlite3_int64) received a literal 0 as one word without this. */
static int ps_parse_fp_params(int *np_out) {
    int ptys[64];
    int pctys[64];
    int pcty;
    int np;
    int ty;
    int var;
    int base;
    int outer_cty;
    /* The parameters' types are read with parse_type, which sets
     * ps_type_cty; the declarator they belong to is still using it.  An
     * unnamed fn-pointer parameter after the first took its last
     * parameter's type -- SQLite's sqlite3_result_text(..., void(*)(void*))
     * had an int where the destructor goes. */
    outer_cty = ps_type_cty;
    np = 0;
    var = 0;
    *np_out = 0;
    if (lex_tok == TK_RPAREN) { next(); ps_type_cty = outer_cty; return ps_fp_store(ptys, pctys, 0); }
    while (lex_tok != TK_EOF) {
        if (lex_tok == TK_ELLIPSIS) { next(); var = 1; break; }
        if (!is_type()) { p_skip_to_rparen(); ps_type_cty = outer_cty; return -1; }
        ty = parse_type();
        skip_decl_qualifiers();
        ty = ps_declarator_stars(ty, ps_type_cty);
        pcty = ps_type_cty;
        if (lex_tok == TK_LPAREN) {
            /* a function-pointer parameter: (*name)(...) -- a pointer */
            next();
            p_skip_to_rparen();
            if (lex_tok == TK_LPAREN) { next(); p_skip_to_rparen(); }
            ty = TY_INT + TY_PTR;
            pcty = ct_ptr(ct_func(pcty, 0, -1, 0));
        } else {
            if (lex_tok == TK_IDENT) next();
            while (lex_tok == TK_LBRACK) {
                next();
                while (lex_tok != TK_RBRACK && lex_tok != TK_EOF) next();
                next();
                ty = ty + TY_PTR;    /* an array parameter is a pointer */
                pcty = ct_ptr(pcty);
            }
        }
        if (np < 64) { ptys[np] = ty; pctys[np] = pcty; np = np + 1; }
        if (lex_tok == TK_COMMA) { next(); continue; }
        break;
    }
    while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next();
    if (lex_tok == TK_RPAREN) next();
    if (np == 1 && ptys[0] == TY_VOID) np = 0;
    *np_out = np;
    base = ps_fp_store(ptys, pctys, np);
    /* A call through a variadic signature packs its doubles the way a
     * direct call to a variadic function does (hl_expr, ND_CALL_PTR). */
    if (base >= 0 && base < PS_MAX_FPARAM) ps_fpvar[base] = var;
    ps_type_cty = outer_cty;
    return base;
}

static void expect(int tok) {
    if (lex_tok != tok) {
        pp_where();
        fdputs("error: expected token ", 2);
        fdputuint(2, tok);
        fdputs(" got ", 2);
        fdputuint(2, lex_tok);
        fdputc(10, 2);
        p_show_line();
        exit(1);
    }
    next();
}

static int is_type(void) {
    if (lex_tok == TK_INT) return 1;
    if (lex_tok == TK_REGISTER) return 1;   /* register int x, a parameter's too (6.7.6.3p2) */
    if (lex_tok == TK_VOID) return 1;
    if (lex_tok == TK_CHAR) return 1;
    if (lex_tok == TK_STRUCT) return 1;
    if (lex_tok == TK_UNION) return 1;
    if (lex_tok == TK_ENUM) return 1;
    if (lex_tok == TK_UNSIGNED) return 1;
    if (lex_tok == TK_SIGNED) return 1;
    if (lex_tok == TK_LONG) return 1;
    if (lex_tok == TK_SHORT) return 1;
    if (lex_tok == TK_FLOAT) return 1;
    if (lex_tok == TK_DOUBLE) return 1;
    if (lex_tok == TK_CONST) return 1;
    if (lex_tok == TK_VOLATILE) return 1;
    if (gnu_float_ident_ty() >= 0) return 1;
    if (is_c11_atomic_ident()) return 1;
    if (is_gnu_extension_ident()) return 1;
    if (is_gnu_qual_ident()) return 1;
    if (is_gnu_attr_ident()) return 1;
    if (is_gnu_typeof_ident()) return 1;
    if (lex_tok == TK_IDENT && find_typedef(lex_str) >= 0) {
        /* ... unless the function being parsed has an object of that name
         * in scope, which hides the typedef (C99 6.2.1p4): after
         *     typedef struct { ... } host;
         *     ... { char host[128]; ... x ? y : (host) ... }
         * "(host)" is the array, and was read as a cast.  Only inside a
         * body: the table still holds the last function's parameters
         * while file-scope declarations are read. */
        if (ps_in_body && find_local(lex_str) >= 0) return 0;
        return 1;
    }
    return 0;
}

/* --- Enum helpers --- */

static int find_const(char *name) {
    int i;
    i = ps_nconsts - 1;
    while (i >= 0) {
        if (strcmp(name, ps_cname[i]) == 0) return i;
        i = i - 1;
    }
    return -1;
}

/* --- Typedef helpers --- */

static int find_typedef(char *name) {
    int i;
    i = ps_ntypedefs - 1;
    while (i >= 0) {
        if (strcmp(name, ps_tdname[i]) == 0) return ps_tdtype[i];
        i = i - 1;
    }
    return -1;
}

static void add_typedef(char *name, int ty) {
    if (ps_ntypedefs >= PS_MAX_TYPEDEFS) {
        p_error("too many typedefs");
        return;
    }
    ps_tdname[ps_ntypedefs] = strdup(name);
    ps_tddepth[ps_ntypedefs] = ps_block_depth;
    ps_tdtype[ps_ntypedefs] = ty;
    ps_tdcty[ps_ntypedefs] = ct_from_mty(ty, 0);   /* a caller with better knowledge sets it after */
    ps_tdarr[ps_ntypedefs] = 0;
    ps_tdfpbase[ps_ntypedefs] = -1;
    ps_tdfpn[ps_ntypedefs] = 0;
    ps_ntypedefs = ps_ntypedefs + 1;
}

/* Function return type table helpers */
static int find_func_type(char *name) {
    int i;
    i = ps_nfuncs - 1;
    while (i >= 0) {
        if (strcmp(name, ps_fname[i]) == 0) return ps_ftype[i];
        i = i - 1;
    }
    return TY_INT;  /* default to int if not found */
}

static int is_known_func(char *name) {
    int i;
    i = ps_nfuncs - 1;
    while (i >= 0) {
        if (strcmp(name, ps_fname[i]) == 0) return 1;
        i = i - 1;
    }
    return 0;
}

/* Variadic-ness of a declared function (0 for unknown callees; C
 * defaults unprototyped functions to non-variadic). */
static int find_func_varargs(char *name) {
    int i;
    i = ps_nfuncs - 1;
    while (i >= 0) {
        if (strcmp(name, ps_fname[i]) == 0) return ps_fvar[i];
        i = i - 1;
    }
    return 0;
}

/* A parameter-type list: machine types, and C types beside them (pctys
 * 0: rebuilt from the machine types). */
static int ps_fp_store(int *ptys, int *pctys, int np) {
    int base;
    int i;
    if (np < 0 || ps_nfptypes + np > PS_MAX_FPARAM) return -1;
    base = ps_nfptypes;
    if (base < PS_MAX_FPARAM) ps_fpvar[base] = 0;
    i = 0;
    while (i < np) {
        ps_fptypes[ps_nfptypes] = ptys[i];
        if (pctys) ps_fpctypes[ps_nfptypes] = ct_unqual(pctys[i]);
        else ps_fpctypes[ps_nfptypes] = ct_from_mty(ptys[i], 0);
        ps_nfptypes = ps_nfptypes + 1;
        i = i + 1;
    }
    return base;
}

/* The function type a stored signature describes: ret, the list at
 * base (np entries; np < 0, no prototype), variadic. */
static int ps_fp_cty(int ret_cty, int base, int np, int var) {
    if (base < 0 || np < 0) return ct_func(ret_cty, 0, -1, 0);
    return ct_func(ret_cty, ps_fpctypes + base, np, var);
}

/* Return type of a function-pointer signature.  Indexed by the same
 * base ps_parse_fp_params returns.  Unset (BSS 0) is TY_INT, which is
 * what ND_CALL_PTR used to hard-code — GitHub issue 69. */
static void ps_fp_set_ret(int base, int ret_ty) {
    if (base >= 0 && base < PS_MAX_FPARAM) ps_fpret[base] = ret_ty;
}

static int ps_fp_get_ret(int base) {
    if (base < 0 || base >= PS_MAX_FPARAM) return TY_INT;
    return ps_fpret[base];
}

static int ps_fp_ret_of_callee(Node *lhs) {
    int fpb;
    if (!lhs) return TY_INT;
    fpb = -1;
    if (lhs->kind == ND_MEMBER && lhs->offset >= 0) fpb = lhs->offset;
    else if (lhs->is_fnptr) fpb = lhs->val;
    return ps_fp_get_ret(fpb);
}

/* A redeclaration of function i: its type compatible with the earlier
 * (6.7p4) -- int g(int); int g(double); was taken, the later winning. */
static void ps_func_redecl_check(int i, int ret_cty, int *ptys, int *pctys, int np) {
    int t;
    int k;
    int pc[64];
    if (ps_fcty[i] <= 0) return;
    if (np < 0) {
        t = ct_func(ret_cty, 0, -1, ps_is_varargs);
    } else {
        k = 0;
        while (k < np && k < 64) {
            pc[k] = pctys ? ct_unqual(pctys[k]) : ct_from_mty(ptys[k], 0);
            k = k + 1;
        }
        t = ct_func(ret_cty, pc, k, ps_is_varargs);
    }
    if (!sema_compat(ps_fcty[i], t)) p_error("conflicting types for function");
}

/* A function's body: one per function (6.9p3). */
static void ps_func_defined(char *name) {
    int i;
    i = ps_nfuncs - 1;
    while (i >= 0 && strcmp(name, ps_fname[i]) != 0) i = i - 1;
    if (i < 0) return;
    if (ps_fdefined[i]) p_error("redefinition of function");
    ps_fdefined[i] = 1;
}

static void add_func_type(char *name, int ty, int *ptys, int *pctys, int np, int ret_cty) {
    int i;
    /* a function declarator's own constraints (6.7.6.3p1-p2, 6.7.4p3, 5.1.2.2.1) */
    if (ps_top_inline && strcmp(name, "main") == 0) p_error("'main' is not allowed to be declared inline");
    ps_top_inline = 0;
    ps_top_static = 0;
    if (ret_cty > 0 && (ct_is_array(ret_cty) || ct_is_func(ret_cty)))
        p_error("function cannot return an array or function type");
    i = 0;
    while (i < np) {
        if (ptys[i] == TY_VOID) p_error("'void' must be the first and only parameter if specified");
        i = i + 1;
    }
    /* Update existing entry if already registered */
    i = ps_nfuncs - 1;
    while (i >= 0) {
        if (strcmp(name, ps_fname[i]) == 0) {
            ps_func_redecl_check(i, ret_cty, ptys, pctys, np);
            ps_ftype[i] = ty;
            ps_fvar[i] = ps_is_varargs;
            if (np >= 0) {
                ps_fpbase[i] = ps_fp_store(ptys, pctys, np);
                ps_fpn[i] = (ps_fpbase[i] >= 0) ? np : 0;
            }
            ps_fcty[i] = ps_fp_cty(ret_cty, ps_fpbase[i], ps_fpn[i], ps_is_varargs);
            ps_fretfpbase[i] = ps_pending_retfpbase;
            ps_fretfpn[i] = ps_pending_retfpn;
            return;
        }
        i = i - 1;
    }
    if (ps_nfuncs >= PS_MAX_FUNCS) return;
    ps_fname[ps_nfuncs] = strdup(name);
    ps_fdefined[ps_nfuncs] = 0;
    ps_ftype[ps_nfuncs] = ty;
    ps_fvar[ps_nfuncs] = ps_is_varargs;
    ps_fpbase[ps_nfuncs] = (np >= 0) ? ps_fp_store(ptys, pctys, np) : -1;
    ps_fpn[ps_nfuncs] = (ps_fpbase[ps_nfuncs] >= 0) ? np : 0;
    ps_fcty[ps_nfuncs] = ps_fp_cty(ret_cty, ps_fpbase[ps_nfuncs],
                                   ps_fpbase[ps_nfuncs] >= 0 ? np : -1, ps_is_varargs);
    ps_fretfpbase[ps_nfuncs] = ps_pending_retfpbase;
    ps_fretfpn[ps_nfuncs] = ps_pending_retfpn;
    ps_nfuncs = ps_nfuncs + 1;
}

/* A function's C type, or 0 when it has none recorded. */
static int find_func_cty(char *name) {
    int i;
    i = ps_nfuncs - 1;
    while (i >= 0) {
        if (strcmp(name, ps_fname[i]) == 0) return ps_fcty[i];
        i = i - 1;
    }
    return 0;
}

/* The C type of parameter idx of name, or 0 when unknown. */
static int find_func_param_cty(char *name, int idx) {
    int i;
    i = 0;
    while (i < ps_nfuncs) {
        if (strcmp(name, ps_fname[i]) == 0) {
            if (ps_fpbase[i] < 0 || idx >= ps_fpn[i]) return 0;
            return ps_fpctypes[ps_fpbase[i] + idx];
        }
        i = i + 1;
    }
    return 0;
}

/* Declared type of parameter `idx` of `name`, or -1 when unknown
 * (unregistered function, old-style declaration, or a variadic slot
 * past the fixed parameters). */
static int find_func_param(char *name, int idx) {
    int i;
    i = 0;
    while (i < ps_nfuncs) {
        if (strcmp(name, ps_fname[i]) == 0) {
            if (ps_fpbase[i] < 0) return -1;
            if (idx >= ps_fpn[i]) return -1;
            return ps_fptypes[ps_fpbase[i] + idx];
        }
        i = i + 1;
    }
    return -1;
}

/* Find or create a label for goto/label. Returns codegen label ID. */
static int find_or_add_label(char *name) {
    int i;
    i = 0;
    while (i < ps_nlabels) {
        if (strcmp(name, ps_lblname[i]) == 0) return ps_lblid[i];
        i = i + 1;
    }
    if (ps_nlabels >= P_MAX_LABELS) {
        p_error("too many labels");
        return 0;
    }
    ps_lblname[ps_nlabels] = strdup(name);
    ps_lbldef[ps_nlabels] = 0;
    ps_lblid[ps_nlabels] = cg_label();
    ps_nlabels = ps_nlabels + 1;
    return ps_lblid[ps_nlabels - 1];
}

/* A label's definition: each name once in a function (6.8.1p3) --
 * `here:` in two blocks was the same label twice, the later one taken. */
static int ps_define_label(char *name) {
    int id;
    int i;
    id = find_or_add_label(name);
    i = 0;
    while (i < ps_nlabels && strcmp(name, ps_lblname[i]) != 0) i = i + 1;
    if (i < ps_nlabels) {
        if (ps_lbldef[i]) p_error("redefinition of label");
        ps_lbldef[i] = 1;
        ps_lblvtop[i] = ps_vd_top;
    }
    return id;
}

/* --- Struct helpers --- */

/* Structure and union tags have block scope (6.2.1p4).  st_depth is
 * the block nesting a tag was declared at, 0 for file scope, -1 once its
 * block has closed: the entry stays (types name it by index), only the
 * name is gone. */
static int st_depth[ST_MAX_STRUCTS];

static int find_struct(char *name) {
    int i;
    i = st_count - 1;
    while (i >= 0) {
        if (st_depth[i] >= 0 && strcmp(name, st_name[i]) == 0) return i;
        i = i - 1;
    }
    return -1;
}

/* A definition declares a new type unless the tag is already declared
 * in this same scope: struct t { ... } in a block shadows an outer t. */
static int ps_tag_for_definition(int si) {
    return si >= 0 && st_depth[si] == ps_block_depth;
}

/* A tag looked up for struct (is_union 0) or union (1): one already
 * declared as the other kind is an error where the name refers to it
 * (6.7.2.3p2) -- not where a definition in an inner block declares a
 * new one. */
static int ps_find_tag(char *name, int is_union) {
    int si;
    si = find_struct(name);
    if (si >= 0 && st_is_union[si] != is_union &&
        (lex_tok != TK_LBRACE || st_depth[si] == ps_block_depth))
        p_error("use of tag with a kind that does not match its previous declaration");
    return si;
}

/* A definition of tag si: a new type unless already declared in this
 * scope (ps_tag_for_definition), and not twice there (6.7.2.3p1). */
static int ps_tag_defines_here(int si) {
    if (!ps_tag_for_definition(si)) return 0;
    if (st_size[si] > 0) p_error("redefinition of struct or union");
    return 1;
}

/* No two members of si with one name (6.7.2.1p2, members of anonymous
 * structures and unions included). */
static void ps_check_dup_members(int si) {
    int i;
    int j;
    i = 0;
    while (i < stm_count) {
        if (stm_owner[i] == si && stm_name[i][0] != 0) {
            j = i + 1;
            while (j < stm_count) {
                if (stm_owner[j] == si && strcmp(stm_name[i], stm_name[j]) == 0)
                    p_error("duplicate member");
                j = j + 1;
            }
        }
        i = i + 1;
    }
}

/* The tags declared in the block now closing go out of scope. */
static void ps_end_block_tags(void) {
    int i;
    i = st_count - 1;
    while (i >= 0 && st_depth[i] >= ps_block_depth) {
        st_depth[i] = -1;
        i = i - 1;
    }
}

static int add_struct(char *name) {
    int idx;
    if (st_count >= ST_MAX_STRUCTS) {
        p_error("too many structs");
        return 0;
    }
    idx = st_count;
    st_name[idx] = strdup(name);
    st_nfields[idx] = 0;
    st_first[idx] = stm_count;
    st_size[idx] = 0;
    st_align[idx] = 1;
    st_is_union[idx] = 0;
    st_depth[idx] = ps_block_depth;
    st_count = st_count + 1;
    return idx;
}

static void require_complete_type(int ty, char *msg) {
    if (ty_is_incomplete_struct(ty)) p_error(msg);
}

static void make_anon_tag(char *buf, char *prefix) {
    int i;
    int n;
    int tmp[12];
    int ntmp;
    i = 0;
    while (prefix[i] != 0) {
        buf[i] = prefix[i];
        i = i + 1;
    }
    n = st_count;
    ntmp = 0;
    if (n == 0) {
        tmp[ntmp] = 0;
        ntmp = ntmp + 1;
    } else {
        while (n > 0) {
            tmp[ntmp] = n % 10;
            ntmp = ntmp + 1;
            n = n / 10;
        }
    }
    while (ntmp > 0) {
        ntmp = ntmp - 1;
        buf[i] = 48 + tmp[ntmp];
        i = i + 1;
    }
    buf[i] = 0;
}

static int find_member(int sty, char *name) {
    int si;
    int i;
    si = ty_struct_idx(sty);
    i = 0;
    while (i < stm_count) {
        if (stm_owner[i] == si && stm_name[i][0] != 0 &&
            strcmp(name, stm_name[i]) == 0) return i;
        i = i + 1;
    }
    return -1;
}

static int struct_field_nth_idx(int si, int nth) {
    int i;
    int seen;
    i = 0;
    seen = 0;
    while (i < stm_count) {
        if (stm_owner[i] == si && !stm_synth[i]) {
            if (seen == nth) return i;
            seen = seen + 1;
        }
        i = i + 1;
    }
    return -1;
}

static int struct_member_extent(int mi) {
    if (stm_is_arr[mi]) return stm_arr_size[mi];
    return ty_size(stm_type[mi]);
}

static int struct_member_array_count(int mi) {
    int elem_ty;
    if (!stm_is_arr[mi]) return 0;
    elem_ty = ty_deref(stm_type[mi]);
    return stm_arr_size[mi] / ty_size(elem_ty);
}

static int struct_member_is_flexible_array(int mi) {
    return stm_is_arr[mi] && stm_arr_size[mi] == 0;
}

static int struct_member_nth_for_idx(int si, int mi) {
    int i;
    int seen;
    int off;
    int end;

    i = 0;
    seen = 0;
    while (i < stm_count) {
        if (stm_owner[i] == si && !stm_synth[i]) {
            if (i == mi) return seen;
            off = stm_off[i];
            end = off + struct_member_extent(i);
            if (stm_off[mi] >= off && stm_off[mi] < end) return seen;
            seen = seen + 1;
        }
        i = i + 1;
    }
    return -1;
}

static void add_anonymous_aggregate_members(int owner_si, int nested_ty, int base_off) {
    int nsi;
    int i;
    int limit;

    require_complete_type(nested_ty, "incomplete anonymous aggregate member");
    if (!ty_is_struct(nested_ty)) {
        p_error("anonymous aggregate member must be struct or union");
        return;
    }
    if (stm_count >= ST_MAX_MEMBERS) {
        p_error("too many struct members");
        return;
    }

    stm_name[stm_count] = strdup("");
    stm_fpbase[stm_count] = -1;
    stm_fpn[stm_count] = 0;
    stm_isfp[stm_count] = 0;
    stm_type[stm_count] = nested_ty;
    stm_vol[stm_count] = ps_type_volb;
    stm_off[stm_count] = base_off;
    stm_is_arr[stm_count] = 0;
                        stm_arr_cols[stm_count] = 0;
    stm_arr_size[stm_count] = 0;
    stm_owner[stm_count] = owner_si;
    stm_synth[stm_count] = 0;
    stm_cty[stm_count] = ps_member_cty(stm_count);
    stm_count = stm_count + 1;
    st_nfields[owner_si] = st_nfields[owner_si] + 1;

    nsi = ty_struct_idx(nested_ty);
    limit = stm_count;
    i = 0;
    while (i < limit) {
        if (stm_owner[i] == nsi && stm_name[i][0] != 0) {
            if (stm_count >= ST_MAX_MEMBERS) {
                p_error("too many struct members");
                return;
            }
            stm_name[stm_count] = strdup(stm_name[i]);
            stm_fpbase[stm_count] = stm_fpbase[i];
            stm_fpn[stm_count] = stm_fpn[i];
            stm_isfp[stm_count] = stm_isfp[i];
            stm_type[stm_count] = stm_type[i];
            stm_cty[stm_count] = stm_cty[i];
            stm_vol[stm_count] = stm_vol[i];
            stm_off[stm_count] = base_off + stm_off[i];
            stm_is_arr[stm_count] = stm_is_arr[i];
            stm_arr_size[stm_count] = stm_arr_size[i];
            stm_arr_cols[stm_count] = stm_arr_cols[i];
            stm_arr_sub[stm_count] = stm_arr_sub[i];
            stm_owner[stm_count] = owner_si;
            stm_synth[stm_count] = 1;
            stm_bit_off[stm_count] = stm_bit_off[i];
            stm_bit_width[stm_count] = stm_bit_width[i];
            stm_count = stm_count + 1;
        }
        i = i + 1;
    }
}

/* --- Bit-field allocation (6.7.2.1p11) ---
 *
 * The layout is clang's, so a structure means the same bytes to both
 * compilers: a field of type T takes the next free bit, low bits first,
 * unless it would cross a boundary T is aligned to -- then it starts at
 * that boundary; `:0` moves to the next boundary of its type.  The bits
 * share bytes with the members around them (`char c; unsigned x : 3;` is
 * 4 bytes, x in the second), and only a named field gives the structure
 * its type's alignment.  The old layout opened a fresh T-aligned unit
 * whenever the type changed or a field did not fit, so
 * `{unsigned x : 3; _Bool f : 1;}` was 8 bytes to clang's 4 (W13).
 *
 * *bfbit, per structure so nested definitions keep their own: the bit
 * after the last bit-field, or -1 when the last member was not one and
 * the next starts at the byte cursor *cur_off.
 *
 * bf_alloc places a field of width bits (0 for `:0`) and returns the
 * byte offset of the T-sized unit the lowering loads and stores, its bit
 * offset within that unit in *bit_off_out; *cur_off becomes the first
 * byte wholly past the bits. */
static int bf_alloc(int dty, int width, int *cur_off, int *bfbit, int *bit_off_out) {
    int ubits;
    int p;
    ubits = ty_align(dty) * 8;
    p = *bfbit >= 0 ? *bfbit : *cur_off * 8;
    if (width == 0 || p / ubits != (p + width - 1) / ubits)
        p = ((p + ubits - 1) / ubits) * ubits;
    *bfbit = p + width;
    *cur_off = (*bfbit + 7) / 8;
    *bit_off_out = p % ubits;
    return (p / ubits) * ty_align(dty);
}

/* Bit-fields are restricted to integer base types no wider than 32 bits.
 * C99 6.7.2.1p9 names _Bool/signed int/unsigned int and "some other
 * implementation-defined type"; char and short are the common extensions.
 * Wider types (long long, __int128, pointers, floats, struct) need an
 * 8+ byte RMW path the lowering pass doesn't have. */
static int bf_is_valid_type(int dty) {
    int base;
    if (ty_is_ptr(dty)) return 0;
    base = dty & TY_BASE_MASK;
    return base == TY_CHAR || base == TY_SHORT || base == TY_INT;
}

/* Parse a type: int, char, void, struct, with optional pointer stars */
static int ps_tdfind(char *nm) {
    int tdi;
    tdi = ps_ntypedefs - 1;
    while (tdi >= 0 && strcmp(nm, ps_tdname[tdi]) != 0) tdi = tdi - 1;
    return tdi;
}

/* parse_type's base C type: a typedef's own, or the machine type read
 * with the hint for what it cannot say, then the base's qualifiers. */
static int ps_base_cty(int ty, int ckhint, int td_cty, int is_const, int is_vol) {
    int c;
    if (td_cty >= 0) c = td_cty;
    else c = ct_from_mty(ty, ckhint);
    if (is_const) c = ct_qualify(c, CQ_CONST);
    if (is_vol) c = ct_qualify(c, CQ_VOLATILE);
    return c;
}

/* A const among a declaration's storage-class words (static const char
 * *z): the declaration loops read it before parse_type runs, and the C
 * type lost it -- the pointee of every such local and global was
 * unqualified.  ps_storage_next steps over one of those words and keeps
 * the const for parse_type. */
static int ps_lead_const;
/* The storage classes of the declaration being read (6.7.1): at most
 * one; neither auto nor register at file scope (6.9p2), a function
 * there included; and register remembered, for & (6.5.3.2p1).  A
 * declaration's parse_type takes them (ps_decl_register). */
static int ps_pend_nstorage;
static int ps_pend_thread;      /* _Thread_local read with the storage class ... */
static int ps_pend_se;          /* ... and static or extern: at block scope it needs one (6.7.1p3) */
static int ps_lead_restrict;    /* restrict before the base type: it must be a pointer (6.7.3p2) */
static int ps_pend_register;
static int ps_decl_register;
static int ps_type_name_cty(void);

/* _Alignas ( type-name ) or _Alignas ( constant-expression ) (6.7.5),
 * the cursor on _Alignas: a power of two, or 0 for none; the strictest
 * of a declaration's is kept in ps_pend_alignas.  A type-name operand
 * runs parse_type, which takes the pending storage-class state: it is
 * kept aside. */
static void ps_read_alignas(void) {
    int a;
    int sv_al;
    int sv_reg;
    int sv_ns;
    int sv_th;
    int sv_se;
    int sv_lc;
    sv_al = ps_pend_alignas;
    sv_reg = ps_pend_register;
    sv_ns = ps_pend_nstorage;
    sv_th = ps_pend_thread;
    sv_se = ps_pend_se;
    sv_lc = ps_lead_const;
    ps_lead_const = 0;
    next();
    expect(TK_LPAREN);
    if (is_type()) a = ct_alignof(ps_type_name_cty());
    else a = parse_const_int();
    expect(TK_RPAREN);
    ps_pend_register = sv_reg;
    ps_pend_nstorage = sv_ns;
    ps_pend_thread = sv_th;
    ps_pend_se = sv_se;
    ps_lead_const = sv_lc;
    if (a < 0 || (a & (a - 1)) != 0) p_error("requested alignment is not a power of 2");
    ps_pend_alignas = a > sv_al ? a : sv_al;
}

/* What _Alignas asks of an object of machine type ty (6.7.5p4): no
 * weaker than its own alignment.  Returns the alignment to give it. */
/* A member's alignment: its type's, or its _Alignas (a). */
static int ps_member_align(int ty, int a) {
    if (a == 0) return ty_align(ty);
    if (a < ty_align(ty)) p_error("requested alignment is less than minimum alignment");
    return a;
}

static int ps_alignas_for(int ty) {
    int nat;
    nat = ty_align(ty);
    if (ps_type_alignas == 0) return nat;
    if (ps_type_alignas < nat) p_error("requested alignment is less than minimum alignment");
    return ps_type_alignas;
}

static void ps_storage_next(void) {
    if (lex_tok == TK_IDENT && strcmp(lex_str, "_Alignas") == 0) {
        ps_read_alignas();
        return;
    }
    if (lex_tok == TK_CONST) ps_lead_const = 1;
    if (lex_tok == TK_STATIC || lex_tok == TK_EXTERN || lex_tok == TK_AUTO || lex_tok == TK_REGISTER) {
        ps_pend_nstorage = ps_pend_nstorage + 1;
        if (ps_pend_nstorage > 1) p_error("cannot combine with previous storage class specifier");
    }
    if ((lex_tok == TK_AUTO || lex_tok == TK_REGISTER) && ps_block_depth == 0)
        p_error("illegal storage class at file scope");
    if (lex_tok == TK_REGISTER) ps_pend_register = 1;
    if (lex_tok == TK_STATIC || lex_tok == TK_EXTERN) ps_pend_se = 1;
    if (lex_tok == TK_IDENT && strcmp(lex_str, "_Thread_local") == 0) ps_pend_thread = 1;
    if (lex_tok == TK_RESTRICT) ps_lead_restrict = 1;
    if (lex_tok == TK_INLINE && ps_block_depth == 0) ps_top_inline = 1;
    if (lex_tok == TK_STATIC && ps_block_depth == 0) ps_top_static = 1;
    next();
}

/* _Atomic on an array or a function type (6.7.2.4p3, 6.7.3p3) */
static void ps_atomic_check(void) {
    if (ps_type_arrcount > 0 || (ps_type_cty > 0 && (ct_is_array(ps_type_cty) || ct_is_func(ps_type_cty))))
        p_error("_Atomic cannot be applied to an array or function type");
}

/* The arithmetic type specifiers, in any order (6.7.2p2): int signed,
 * int long long, short unsigned int -- each was read in one order only.
 * signed_seen: a `signed` already read before them.  The C type's hint
 * for what the machine type cannot say goes in *hint. */
static int ps_arith_specifiers(int signed_seen, int *hint) {
    int nint;
    int nchar;
    int nshort;
    int nlong;
    int nsig;
    int nuns;
    int ndbl;
    nint = 0;
    nchar = 0;
    nshort = 0;
    nlong = 0;
    nsig = signed_seen;
    nuns = 0;
    ndbl = 0;
    while (1) {
        if (lex_tok == TK_INT) nint = nint + 1;
        else if (lex_tok == TK_CHAR) nchar = nchar + 1;
        else if (lex_tok == TK_SHORT) nshort = nshort + 1;
        else if (lex_tok == TK_LONG) nlong = nlong + 1;
        else if (lex_tok == TK_SIGNED) nsig = nsig + 1;
        else if (lex_tok == TK_UNSIGNED) nuns = nuns + 1;
        else if (lex_tok == TK_DOUBLE && nlong == 1) ndbl = ndbl + 1;
        else break;
        next();
    }
    if (nsig && nuns) p_error("cannot combine signed and unsigned");
    if (nint > 1 || nchar > 1 || nshort > 1 || nlong > 2 || (nchar && (nshort || nlong || nint)) ||
        (nshort && nlong))
        p_error("invalid combination of type specifiers");
    if (ndbl) { *hint = CT_LDOUBLE; return TY_DOUBLE; }
    if (nchar) {
        if (nuns) return TY_CHAR | TY_UNSIGNED;
        if (nsig) *hint = CT_SCHAR;
        return TY_CHAR;
    }
    if (nshort) return nuns ? TY_SHORT | TY_UNSIGNED : TY_SHORT;
    if (nlong == 2) return nuns ? TY_LLONG | TY_UNSIGNED : TY_LLONG;
    if (nlong == 1) {
        *hint = nuns ? CT_ULONG : CT_LONG;
        return nuns ? target_long_ty() | TY_UNSIGNED : target_long_ty();
    }
    return nuns ? TY_INT | TY_UNSIGNED : TY_INT;
}

static int parse_type(void) {
    int malignas;       /* a member declaration's _Alignas */
    int saw_signed;
    int lead_const;
    int mcty;            /* a member declaration's C type ... */
    int mbase_cty;       /* ... and its base, for the declarators after a comma */
    int ckhint;          /* a CT_ kind the int encoding loses: long, signed char, long double, enum */
    int td_cty;          /* the base came from a typedef: its C type */
    int ty;
    int si;
    int mty;
    int dty;
    int off;
    int max_sz;
    int arr_count;
    int arr_ndims;
    int arr_last;
    int arr_dims[16];      /* a member array's dimensions, as written */
    int arr_sub;
    int val;
    int flex_member;
    int first_decl;
    char nm[256];
    int mtdac;
    mtdac = 0;
    (void)mtdac;
    ps_type_arrcount = 0;
    ps_type_fpbase = -1;
    ps_type_fpn = 0;
    ps_type_volb = 0;
    ps_type_volp = 0;
    lead_const = ps_lead_const;     /* a const read with the storage class */
    ps_lead_const = 0;
    ps_decl_register = ps_pend_register;
    ps_pend_register = 0;
    ps_pend_nstorage = 0;
    ps_type_alignas = ps_pend_alignas;
    ps_pend_alignas = 0;
    if (ps_pend_thread && ps_block_depth > 0 && !ps_pend_se)
        p_error("_Thread_local at block scope requires static or extern");
    ps_pend_thread = 0;
    ps_pend_se = 0;
    ckhint = 0;
    td_cty = -1;
    saw_signed = 0;
    /* Skip const/volatile/signed/restrict qualifiers */
    while (1) {
        if (lex_tok == TK_IDENT && strcmp(lex_str, "_Alignas") == 0) {
            ps_pend_alignas = ps_type_alignas;
            ps_read_alignas();
            ps_type_alignas = ps_pend_alignas;
            ps_pend_alignas = 0;
            continue;
        }
        if (lex_tok == TK_REGISTER) {   /* a parameter's: its address may not be taken */
            ps_decl_register = 1;
            next();
            continue;
        }
        if (lex_tok == TK_CONST || lex_tok == TK_VOLATILE ||
            lex_tok == TK_SIGNED || lex_tok == TK_RESTRICT ||
            is_gnu_qual_ident() || is_gnu_extension_ident() ||
            is_gnu_inline_ident()) {
            if (lex_tok == TK_SIGNED) saw_signed = 1;
            if (ps_tok_volatile()) ps_type_volb = 1;
            if (lex_tok == TK_CONST) lead_const = 1;
            if (lex_tok == TK_RESTRICT) ps_lead_restrict = 1;
            next();
            continue;
        }
        if (is_gnu_attr_ident()) {
            skip_gnu_attributes();
            continue;
        }
        break;
    }
    if (is_c11_atomic_ident()) {
        next();
        if (lex_tok == TK_LPAREN) {
            next();
            ty = parse_type();
            ps_atomic_check();
            expect(TK_RPAREN);
            ps_type_nostar = ty;
            while (lex_tok == TK_STAR) { ty = ty + TY_PTR; next(); skip_decl_qualifiers(); }
            /* never const: the bits are the parenthesised type's, and
             * these stars were not counted (ISSUES-99) */
            ps_type_cty = ct_from_mty(ty, 0);
            ps_type_constb = 0;
            ps_type_nstar = 0;
            ps_type_constp = 0;
            return ty;
        }
        ty = parse_type();
        ps_atomic_check();
        return ty;
    }
    if (lex_tok == TK_INT || lex_tok == TK_CHAR || lex_tok == TK_SHORT ||
        lex_tok == TK_UNSIGNED || lex_tok == TK_LONG) {
        ty = ps_arith_specifiers(saw_signed, &ckhint);
    }
    else if (lex_tok == TK_VOID) { ty = TY_VOID; next(); }
    else if (lex_tok == TK_FLOAT) { ty = TY_FLOAT; next(); }
    else if (lex_tok == TK_DOUBLE) { ty = TY_DOUBLE; next(); }
    else if (gnu_float_ident_ty() >= 0) {
        ty = gnu_float_ident_ty();
        next();
    }
    else if (is_gnu_typeof_ident()) {
        Node *tn;
        next();
        expect(TK_LPAREN);
        if (is_type()) {
            ty = parse_type();
        } else {
            tn = parse_expr();
            ty = tn->ty;
        }
        expect(TK_RPAREN);
    }
    else if (lex_tok == TK_ENUM) {
        next();
        if (lex_tok == TK_IDENT) next();
        if (lex_tok == TK_LBRACE) {
            expect(TK_LBRACE);
            val = 0;
            while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                if (lex_tok != TK_IDENT) p_error("expected enum constant name");
                if (ps_nconsts >= PS_MAX_CONSTS) p_error("too many enum constants");
                ps_cname[ps_nconsts] = strdup(lex_str);
                next();
                if (lex_tok == TK_ASSIGN) {
                    next();
                    val = ps_enum_value();
                }
                ps_cval[ps_nconsts] = val;
                ps_nconsts = ps_nconsts + 1;
                val = val + 1;
                if (lex_tok == TK_COMMA) next();
            }
            expect(TK_RBRACE);
        }
        ty = TY_INT;
        ckhint = CT_ENUM;
    }
    else if (lex_tok == TK_STRUCT) {
        next();
        if (lex_tok == TK_IDENT) {
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            si = ps_find_tag(nm, 0);
        } else if (lex_tok == TK_LBRACE) {
            make_anon_tag(nm, "__anon_struct_");
            si = -1;
        } else {
            p_error("expected struct tag name");
            return TY_INT;
        }
        if (lex_tok == TK_LBRACE) {
            /* Struct definition: struct Name { ... } */
            int max_align;
            int bfbit;      /* the bit-field cursor; see bf_alloc */
            next();
            if (!ps_tag_defines_here(si)) {
                si = add_struct(nm);
            } else if (st_nfields[si] == 0 && st_size[si] == 0) {
                st_first[si] = stm_count;
            }
            off = 0;
            max_align = 1;
            bfbit = -1;
            while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                if (lex_tok == TK_STATIC_ASSERT) { ps_member_static_assert(); continue; }
                mty = parse_type();
                malignas = ps_type_alignas;   /* before an array size runs parse_type again */
                mcty = ps_type_cty;
                mbase_cty = ps_type_nostar_cty;
                mtdac = ps_type_arrcount;
                first_decl = 1;
                flex_member = 0;
                while (1) {
                    int is_fn_ptr_member;
                    int member_ty;
                    int bf_width;
                    int bf_byte_off;
                    int bf_bit_off;
                    int ptr_row_cols;
                    int td_inner;

                    if (first_decl) {
                        dty = mty;
                        ps_type_cty = mcty;
                        first_decl = 0;
                    } else {
                        dty = mty;
                        while (ty_is_ptr(dty)) dty = ty_deref(dty);
                        dty = ps_declarator_stars(dty, mbase_cty);
                    }
                    /* Anonymous bit-field: 'type :W;' — reserves bits, no
                     * member entry; ':0' flushes the current storage unit. */
                    if (lex_tok == TK_COLON) {
                        next();
                        bf_width = parse_const_int();
                        require_complete_type(dty, "incomplete bit-field type");
                        if (!bf_is_valid_type(dty)) {
                            p_error("bit-field must have char/short/int base type");
                            return TY_INT;
                        }
                        if (bf_width < 0) {
                            p_error("negative bit-field width");
                            return TY_INT;
                        }
                        if (bf_width > ty_size(dty) * 8) {
                            p_error("bit-field width exceeds type size");
                            return TY_INT;
                        }
                        bf_alloc(dty, bf_width, &off, &bfbit, &bf_bit_off);
                        goto struct_comma_check;
                    }
                    is_fn_ptr_member = 0;
                    ptr_row_cols = 0;
                    member_ty = dty;
                    skip_gnu_decl_suffixes();
                    if (lex_tok == TK_SEMI && ty_is_struct(member_ty)) {
                        int malign;
                        require_complete_type(member_ty, "incomplete anonymous struct member");
                        bfbit = -1;
                        malign = ty_align(member_ty);
                        if (malign > 1)
                            off = ((off + malign - 1) / malign) * malign;
                        if (malign > max_align) max_align = malign;
                        add_anonymous_aggregate_members(si, member_ty, off);
                        off = off + ty_size(member_ty);
                        break;
                    }
                    if (lex_tok == TK_LPAREN) {
                        next();
                        ps_skip_fnptr_stars();   /* (**name)(...) */
                        /* T (*(*name)(params))(params): a pointer to a
                         * function returning a function pointer (SQLite's
                         * sqlite3_vfs.xDlSym).  Both are opaque pointers
                         * here; the inner ( * name ) ( params ) is taken
                         * now and the outer ) ( params ) below. */
                        if (lex_tok == TK_LPAREN) {
                            next();
                            ps_skip_fnptr_stars();   /* (**name)(...) */
                            is_fn_ptr_member = 2;
                        } else {
                            is_fn_ptr_member = 1;
                        }
                        /* (*(*name)(..))(..): the function returns a pointer to a function */
                        ps_pending_member_cty = ps_fnptr_cty(is_fn_ptr_member == 2 ?
                            ct_ptr(ct_func(ps_type_cty, 0, -1, 0)) : ps_type_cty);
                        if (lex_tok != TK_IDENT) {
                            p_error("expected member name");
                            return TY_INT;
                        }
                        member_ty = TY_PTR + TY_INT;
                    }
                    if (lex_tok != TK_IDENT) {
                        p_error("expected member name");
                        return TY_INT;
                    }
                    if (stm_count >= ST_MAX_MEMBERS) {
                        p_error("too many struct members");
                        return TY_INT;
                    }
                    stm_name[stm_count] = strdup(lex_str);
                    stm_fpbase[stm_count] = -1;
                    stm_fpn[stm_count] = 0;
                    stm_isfp[stm_count] = is_fn_ptr_member != 0;
                    next();
                    if (is_fn_ptr_member == 2) {
                        /* T (*(*name)(callparams))(...): the list right
                         * after the name is the one a call through the
                         * member uses, so record it.  Skipping it left
                         * sqlite3_vfs.xDlSym with no signature, and an
                         * i64 argument went one word (GitHub issue 41). */
                        expect(TK_RPAREN);
                        expect(TK_LPAREN);
                        stm_fpbase[stm_count] = ps_parse_fp_params(&stm_fpn[stm_count]);
                        ps_fp_set_ret(stm_fpbase[stm_count], TY_PTR + TY_INT);
                    }
                    if (is_fn_ptr_member) {
                        expect(TK_RPAREN);
                        if (lex_tok == TK_LPAREN) {
                            next();
                            if (is_fn_ptr_member == 1) {
                                stm_fpbase[stm_count] = ps_parse_fp_params(&stm_fpn[stm_count]);
                                ps_fp_set_ret(stm_fpbase[stm_count], dty);
                            } else
                                p_skip_to_rparen();
                        } else if (lex_tok == TK_LBRACK) {
                            /* Pointer-to-array member: T (*name)[N] — a
                             * 4-byte pointer whose [i] selects row i
                             * (base + i*N*sizeof(T)) with no element
                             * load (sbasic's char (*varnames)[64]). */
                            next();
                            ptr_row_cols = parse_const_int();
                            expect(TK_RBRACK);
                            member_ty = dty + TY_PTR;
                        }
                    }
                    /* Named bit-field: 'type name :W;'.  Must run BEFORE the
                     * non-bit-field block below, which ends the run of bits. */
                    if (!is_fn_ptr_member && lex_tok == TK_COLON) {
                        if (malignas) p_error("_Alignas may not be applied to a bit-field");
                        next();
                        bf_width = parse_const_int();
                        require_complete_type(dty, "incomplete bit-field type");
                        if (!bf_is_valid_type(dty)) {
                            p_error("bit-field must have char/short/int base type");
                            return TY_INT;
                        }
                        if (bf_width <= 0) {
                            p_error("named bit-field must have positive width");
                            return TY_INT;
                        }
                        if (bf_width > ty_size(dty) * 8) {
                            p_error("bit-field width exceeds type size");
                            return TY_INT;
                        }
                        bf_byte_off = bf_alloc(dty, bf_width, &off, &bfbit, &bf_bit_off);
                        if (ty_align(dty) > max_align) max_align = ty_align(dty);
                        stm_owner[stm_count] = si;
                        stm_type[stm_count] = dty;
                        stm_vol[stm_count] = ps_type_volb;
                        stm_is_arr[stm_count] = 0;
                        stm_arr_cols[stm_count] = 0;
                        stm_arr_size[stm_count] = 0;
                        stm_off[stm_count] = bf_byte_off;
                        stm_synth[stm_count] = 0;
                        stm_bit_off[stm_count] = bf_bit_off;
                        stm_bit_width[stm_count] = bf_width;
                        stm_cty[stm_count] = ps_member_cty(stm_count);
                        stm_count = stm_count + 1;
                        st_nfields[si] = st_nfields[si] + 1;
                        skip_gnu_decl_suffixes();
                        goto struct_comma_check;
                    }
                    /* Past the bit-field check: this is a regular member.
                     * It ends a run of bit-fields (off is already past
                     * their bits) and aligns off to its own alignment. */
                    bfbit = -1;
                    /* Check for array member: type name[N][M]...;  A final
                     * unsized member is a C99 flexible array member: it has
                     * array-address semantics but contributes zero bytes to
                     * sizeof(struct). */
                    arr_count = 0;
                    arr_ndims = 0;
                    arr_last = 0;
                    td_inner = 0;
                    if (mtdac > 0 && lex_tok != TK_LBRACK) {
                        /* typedef byte sha1_digest_t[20]; member of that
                         * type is a real array member (net_defs). */
                        arr_count = mtdac;
                        arr_dims[0] = mtdac;
                        arr_ndims = 1;
                        member_ty = ty_deref(member_ty);
                    } else if (mtdac > 0 && member_ty == mty) {
                        /* ...and an array of it -- jmp_buf handlers[4] --
                         * is a two-dimensional array of its element type,
                         * the typedef's dimension the inner one.  (It was
                         * taken for an array of pointers.) */
                        td_inner = mtdac;
                        member_ty = ty_deref(member_ty);
                    }
                    /* Aligned as its element type: a typedef'd array was
                     * still spelled as a pointer here, and an unsigned
                     * char[16] member took a pointer's 4 (SQLite's
                     * yDbMask; 6.7.8-typedef-array-align). */
                    {
                        int malign = ps_member_align(member_ty, malignas);
                        if (malign > 1)
                            off = ((off + malign - 1) / malign) * malign;
                        if (malign > max_align) max_align = malign;
                    }
                    while (lex_tok == TK_LBRACK) {
                        next();
                        if (lex_tok == TK_RBRACK) {
                            if (arr_count != 0 || flex_member) {
                                p_error("invalid flexible array member");
                                return TY_INT;
                            }
                            flex_member = 1;
                            arr_count = 1;
                            next();
                            if (lex_tok == TK_LBRACK) {
                                p_error("invalid flexible array member");
                                return TY_INT;
                            }
                            break;
                        }
                        if (arr_count == 0) arr_count = 1;
                        arr_last = parse_const_int();
                        arr_count = arr_count * arr_last;
                        if (arr_ndims < 16) arr_dims[arr_ndims] = arr_last;
                        arr_ndims = arr_ndims + 1;
                        expect(TK_RBRACK);
                    }
                    if (td_inner > 0 && !flex_member) {
                        arr_count = arr_count * td_inner;
                        arr_last = td_inner;
                        if (arr_ndims < 16) arr_dims[arr_ndims] = arr_last;
                        arr_ndims = arr_ndims + 1;
                    }
                    /* past the first dimension: the elements the first
                     * subscript steps over, and the strides below */
                    arr_sub = 0;
                    if (arr_ndims > 16) {
                        p_error("too many array dimensions");
                        return TY_INT;
                    }
                    if (arr_ndims >= 2) arr_last = ps_dims_shape(&arr_dims[1], arr_ndims - 1, &arr_sub);
                    skip_gnu_decl_suffixes();
                    if (arr_count > 0) {
                        require_complete_type(member_ty, "incomplete struct member");
                        if (flex_member) {
                            if (st_nfields[si] == 0) {
                                p_error("flexible array member requires previous member");
                                return TY_INT;
                            }
                            if (lex_tok != TK_SEMI) {
                                p_error("flexible array member must be last");
                                return TY_INT;
                            }
                        }
                        stm_owner[stm_count] = si;
                        stm_type[stm_count] = member_ty + TY_PTR;
                        stm_vol[stm_count] = ps_type_volb;
                        stm_is_arr[stm_count] = 1;
                        stm_arr_cols[stm_count] = (arr_ndims >= 2) ? arr_last : 0;
                        stm_arr_sub[stm_count] = arr_sub;
                        stm_arr_size[stm_count] = flex_member ? 0 : ty_size(member_ty) * arr_count;
                        stm_off[stm_count] = off;
                        stm_synth[stm_count] = 0;
                        if (!flex_member) off = off + ty_size(member_ty) * arr_count;
                    } else {
                        require_complete_type(member_ty, "incomplete struct member");
                        stm_owner[stm_count] = si;
                        stm_type[stm_count] = member_ty;
                        stm_vol[stm_count] = ps_type_volb;
                        stm_is_arr[stm_count] = 0;
                        stm_arr_cols[stm_count] = ptr_row_cols;
                        stm_arr_sub[stm_count] = 0;
                        stm_arr_size[stm_count] = 0;
                        stm_off[stm_count] = off;
                        stm_synth[stm_count] = 0;
                        off = off + ty_size(member_ty);
                    }
                    stm_cty[stm_count] = ps_member_cty(stm_count);
                    stm_count = stm_count + 1;
                    st_nfields[si] = st_nfields[si] + 1;
                struct_comma_check:
                    if (lex_tok != TK_COMMA) break;
                    next();
                }
                expect(TK_SEMI);
                if (flex_member && lex_tok != TK_RBRACE) {
                    p_error("flexible array member must be last");
                    return TY_INT;
                }
            }
            expect(TK_RBRACE);
            /* The byte cursor `off` is past any trailing bit-fields
             * (bf_alloc keeps it there), so sizeof is correct. */
            /* Round total size up to the struct's alignment so that arrays
             * of struct keep each element naturally aligned.  The historical
             * "round to 4" rule is a special case of this when max_align <= 4. */
            st_align[si] = max_align;
            /* Size rounds to the struct's REAL alignment (C semantics,
             * and clang's layout — the milestone-3 ABI ruling).  The old
             * 4-byte floor made doom's all-shorts mappatch_t sizeof 12
             * instead of 10, so mpatch++ strode past the on-disk data
             * and the first multi-patch texture (COMP2) read garbage. */
            st_size[si] = ((off + max_align - 1) / max_align) * max_align;
            ps_check_dup_members(si);
        } else {
            /* Forward declaration or reference: struct Name (no brace) */
            if (si < 0) {
                si = add_struct(nm);
            }
        }
        ty = TY_STRUCT_BASE + si;
    }
    else if (lex_tok == TK_UNION) {
        next();
        if (lex_tok == TK_IDENT) {
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            si = ps_find_tag(nm, 1);
        } else if (lex_tok == TK_LBRACE) {
            make_anon_tag(nm, "__anon_union_");
            si = -1;
        } else {
            p_error("expected union tag name");
            return TY_INT;
        }
        if (lex_tok == TK_LBRACE) {
            /* Union definition: union Name { ... } */
            int max_align;
            next();
            if (!ps_tag_defines_here(si)) {
                si = add_struct(nm);
            } else if (st_nfields[si] == 0 && st_size[si] == 0) {
                st_first[si] = stm_count;
            }
            st_is_union[si] = 1;
            max_sz = 0;
            max_align = 1;
            while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                if (lex_tok == TK_STATIC_ASSERT) { ps_member_static_assert(); continue; }
                mty = parse_type();
                malignas = ps_type_alignas;   /* before an array size runs parse_type again */
                mcty = ps_type_cty;
                mbase_cty = ps_type_nostar_cty;
                mtdac = ps_type_arrcount;
                first_decl = 1;
                while (1) {
                    int is_fn_ptr_member;
                    int member_ty;
                    int bf_width;
                    int ptr_row_cols;
                    int td_inner;

                    if (first_decl) {
                        dty = mty;
                        ps_type_cty = mcty;
                        first_decl = 0;
                    } else {
                        dty = mty;
                        while (ty_is_ptr(dty)) dty = ty_deref(dty);
                        dty = ps_declarator_stars(dty, mbase_cty);
                    }
                    /* Anonymous bit-field in union: 'type :W;' — affects
                     * sizeof / alignment only.  Unions don't pack, so
                     * there's no unit cursor to track. */
                    if (lex_tok == TK_COLON) {
                        next();
                        bf_width = parse_const_int();
                        require_complete_type(dty, "incomplete bit-field type");
                        if (!bf_is_valid_type(dty)) {
                            p_error("bit-field must have char/short/int base type");
                            return TY_INT;
                        }
                        if (bf_width < 0) {
                            p_error("negative bit-field width");
                            return TY_INT;
                        }
                        if (bf_width > ty_size(dty) * 8) {
                            p_error("bit-field width exceeds type size");
                            return TY_INT;
                        }
                        if (bf_width > 0) {
                            int malign;
                            if (ty_size(dty) > max_sz) max_sz = ty_size(dty);
                            malign = ty_align(dty);
                            if (malign > max_align) max_align = malign;
                        }
                        goto union_comma_check;
                    }
                    is_fn_ptr_member = 0;
                    ptr_row_cols = 0;
                    member_ty = dty;
                    skip_gnu_decl_suffixes();
                    if (lex_tok == TK_SEMI && ty_is_struct(member_ty)) {
                        int malign;
                        require_complete_type(member_ty, "incomplete anonymous union member");
                        add_anonymous_aggregate_members(si, member_ty, 0);
                        if (ty_size(member_ty) > max_sz) max_sz = ty_size(member_ty);
                        malign = ty_align(member_ty);
                        if (malign > max_align) max_align = malign;
                        break;
                    }
                    if (lex_tok == TK_LPAREN) {
                        next();
                        ps_skip_fnptr_stars();   /* (**name)(...) */
                        /* T (*(*name)(params))(params): a pointer to a
                         * function returning a function pointer (SQLite's
                         * sqlite3_vfs.xDlSym).  Both are opaque pointers
                         * here; the inner ( * name ) ( params ) is taken
                         * now and the outer ) ( params ) below. */
                        if (lex_tok == TK_LPAREN) {
                            next();
                            ps_skip_fnptr_stars();   /* (**name)(...) */
                            is_fn_ptr_member = 2;
                        } else {
                            is_fn_ptr_member = 1;
                        }
                        /* (*(*name)(..))(..): the function returns a pointer to a function */
                        ps_pending_member_cty = ps_fnptr_cty(is_fn_ptr_member == 2 ?
                            ct_ptr(ct_func(ps_type_cty, 0, -1, 0)) : ps_type_cty);
                        if (lex_tok != TK_IDENT) {
                            p_error("expected member name");
                            return TY_INT;
                        }
                        member_ty = TY_PTR + TY_INT;
                    }
                    if (lex_tok != TK_IDENT) {
                        p_error("expected member name");
                        return TY_INT;
                    }
                    if (stm_count >= ST_MAX_MEMBERS) {
                        p_error("too many struct members");
                        return TY_INT;
                    }
                    stm_name[stm_count] = strdup(lex_str);
                    stm_fpbase[stm_count] = -1;
                    stm_fpn[stm_count] = 0;
                    stm_isfp[stm_count] = is_fn_ptr_member != 0;
                    next();
                    if (is_fn_ptr_member == 2) {
                        /* T (*(*name)(callparams))(...): the list right
                         * after the name is the one a call through the
                         * member uses, so record it.  Skipping it left
                         * sqlite3_vfs.xDlSym with no signature, and an
                         * i64 argument went one word (GitHub issue 41). */
                        expect(TK_RPAREN);
                        expect(TK_LPAREN);
                        stm_fpbase[stm_count] = ps_parse_fp_params(&stm_fpn[stm_count]);
                        ps_fp_set_ret(stm_fpbase[stm_count], TY_PTR + TY_INT);
                    }
                    if (is_fn_ptr_member) {
                        expect(TK_RPAREN);
                        if (lex_tok == TK_LPAREN) {
                            next();
                            if (is_fn_ptr_member == 1) {
                                stm_fpbase[stm_count] = ps_parse_fp_params(&stm_fpn[stm_count]);
                                ps_fp_set_ret(stm_fpbase[stm_count], dty);
                            } else
                                p_skip_to_rparen();
                        } else if (lex_tok == TK_LBRACK) {
                            /* Pointer-to-array member: T (*name)[N] — a
                             * 4-byte pointer whose [i] selects row i
                             * (base + i*N*sizeof(T)) with no element
                             * load (sbasic's char (*varnames)[64]). */
                            next();
                            ptr_row_cols = parse_const_int();
                            expect(TK_RBRACK);
                            member_ty = dty + TY_PTR;
                        }
                    }
                    /* Named bit-field in union: each occupies bits 0..W-1
                     * at offset 0 (no packing across members). */
                    if (!is_fn_ptr_member && lex_tok == TK_COLON) {
                        if (malignas) p_error("_Alignas may not be applied to a bit-field");
                        int malign;
                        next();
                        bf_width = parse_const_int();
                        require_complete_type(dty, "incomplete bit-field type");
                        if (!bf_is_valid_type(dty)) {
                            p_error("bit-field must have char/short/int base type");
                            return TY_INT;
                        }
                        if (bf_width <= 0) {
                            p_error("named bit-field must have positive width");
                            return TY_INT;
                        }
                        if (bf_width > ty_size(dty) * 8) {
                            p_error("bit-field width exceeds type size");
                            return TY_INT;
                        }
                        stm_owner[stm_count] = si;
                        stm_type[stm_count] = dty;
                        stm_vol[stm_count] = ps_type_volb;
                        stm_is_arr[stm_count] = 0;
                        stm_arr_cols[stm_count] = 0;
                        stm_arr_size[stm_count] = 0;
                        stm_off[stm_count] = 0;
                        stm_synth[stm_count] = 0;
                        stm_bit_off[stm_count] = 0;
                        stm_bit_width[stm_count] = bf_width;
                        stm_cty[stm_count] = ps_member_cty(stm_count);
                        stm_count = stm_count + 1;
                        st_nfields[si] = st_nfields[si] + 1;
                        if (ty_size(dty) > max_sz) max_sz = ty_size(dty);
                        malign = ty_align(dty);
                        if (malign > max_align) max_align = malign;
                        skip_gnu_decl_suffixes();
                        goto union_comma_check;
                    }
                    /* Check for array member: type name[N][M]...; a
                     * typedef'd array is one too, its element type the
                     * member's (it was a pointer, and a union of a
                     * char[16] typedef was 4 bytes). */
                    arr_count = 0;
                    if (mtdac > 0 && member_ty == mty) {
                        arr_count = mtdac;
                        member_ty = ty_deref(member_ty);
                    }
                    while (lex_tok == TK_LBRACK) {
                        next();
                        if (lex_tok == TK_RBRACK) {
                            p_error("array size required in union member");
                            return TY_INT;
                        }
                        if (arr_count == 0) arr_count = 1;
                        arr_count = arr_count * parse_const_int();
                        expect(TK_RBRACK);
                    }
                    skip_gnu_decl_suffixes();
                    if (arr_count > 0) {
                        int malign;
                        require_complete_type(member_ty, "incomplete union member");
                        stm_owner[stm_count] = si;
                        stm_type[stm_count] = member_ty + TY_PTR;
                        stm_vol[stm_count] = ps_type_volb;
                        stm_is_arr[stm_count] = 1;
                        stm_arr_cols[stm_count] = 0;
                        stm_arr_size[stm_count] = ty_size(member_ty) * arr_count;
                        stm_off[stm_count] = 0;
                        stm_synth[stm_count] = 0;
                        if (ty_size(member_ty) * arr_count > max_sz) max_sz = ty_size(member_ty) * arr_count;
                        malign = ps_member_align(member_ty, malignas);
                        if (malign > max_align) max_align = malign;
                    } else {
                        int malign;
                        require_complete_type(member_ty, "incomplete union member");
                        stm_owner[stm_count] = si;
                        stm_type[stm_count] = member_ty;
                        stm_vol[stm_count] = ps_type_volb;
                        stm_is_arr[stm_count] = 0;
                        stm_arr_cols[stm_count] = ptr_row_cols;
                        stm_arr_size[stm_count] = 0;
                        stm_off[stm_count] = 0;
                        stm_synth[stm_count] = 0;
                        if (ty_size(member_ty) > max_sz) max_sz = ty_size(member_ty);
                        malign = ps_member_align(member_ty, malignas);
                        if (malign > max_align) max_align = malign;
                    }
                    stm_cty[stm_count] = ps_member_cty(stm_count);
                    stm_count = stm_count + 1;
                    st_nfields[si] = st_nfields[si] + 1;
                union_comma_check:
                    if (lex_tok != TK_COMMA) break;
                    next();
                }
                expect(TK_SEMI);
            }
            expect(TK_RBRACE);
            /* Round total size up to the union's alignment so that arrays
             * of union keep each element naturally aligned. */
            st_align[si] = max_align;
            st_size[si] = ((max_sz + max_align - 1) / max_align) * max_align;
            ps_check_dup_members(si);
        } else {
            /* Forward declaration or reference: union Name (no brace) */
            if (si < 0) {
                si = add_struct(nm);
                st_is_union[si] = 1;
            }
        }
        ty = TY_STRUCT_BASE + si;
    }
    else if (lex_tok == TK_IDENT && saw_signed && ps_tdfind(lex_str) < 0) {
        /* `signed s = -2;` -- signed alone is int; the identifier is
         * the declarator, not a type. */
        ty = TY_INT;
    }
    else if (lex_tok == TK_IDENT) {
        int tdi;
        tdi = ps_ntypedefs - 1;
        while (tdi >= 0 && strcmp(lex_str, ps_tdname[tdi]) != 0) tdi = tdi - 1;
        if (tdi < 0) { p_error("expected type"); return TY_INT; }
        ty = ps_tdtype[tdi];
        td_cty = ps_tdcty[tdi];
        ps_type_arrcount = ps_tdarr[tdi];
        ps_type_fpbase = ps_tdfpbase[tdi];
        ps_type_fpn = ps_tdfpn[tdi];
        next();
    }
    else if (saw_signed) { ty = TY_INT; }   /* bare `signed`, as in (signed)sizeof(unsigned) */
    else { p_error("expected type"); return TY_INT; }
    /* The const bits are set here, after the base type: a struct or union
     * body above ran parse_type for each member, and a member's const
     * must not become the declaration's. */
    ps_type_constb = lead_const;
    ps_type_nstar = 0;
    ps_type_constp = 0;
    /* qualifiers after the base type: char const *z (SQLite) */
    while (lex_tok == TK_CONST || lex_tok == TK_VOLATILE || is_gnu_qual_ident()) {
        if (ps_tok_volatile()) ps_type_volb = 1;
        if (lex_tok == TK_CONST) ps_type_constb = 1;
        next();
    }
    ps_type_cty = ps_base_cty(ty, ckhint, td_cty, ps_type_constb, ps_type_volb);
    if (ps_lead_restrict && !ct_is_ptr(ps_type_cty))
        p_error("restrict requires a pointer type");
    ps_lead_restrict = 0;
    ps_type_nostar = ty;
    ps_type_nostar_cty = ps_type_cty;
    while (lex_tok == TK_STAR) {
        /* ps_type_fpbase is kept: `dfn *tab` and `dfn tab[N]` reach the
         * side tables with the identical type (elem + TY_PTR, arrays
         * having decayed), so the signature has to survive the star for
         * tab[0](x) and (*tab)(x) to resolve (GitHub issue 70).  It
         * describes what one [] or unary * yields, and is only ever read
         * through those, so a pointer carrying it is not callable. */
        ty = ty + TY_PTR; ps_type_arrcount = 0; next();
        ps_type_nstar = ps_type_nstar + 1;
        ps_type_constp = 0;
        ps_type_cty = ct_ptr(ps_type_cty);
        while (lex_tok == TK_CONST || lex_tok == TK_VOLATILE || lex_tok == TK_RESTRICT || is_gnu_qual_ident()) {   /* u8 * const p */
            if (ps_tok_volatile()) ps_type_volp = 1;
            if (lex_tok == TK_CONST) ps_type_constp = 1;
            ps_type_cty = ct_qualify(ps_type_cty, ps_tok_cq());
            next();
        }
    }
    return ty;
}

/* --- Variable lookup --- */

static int find_local(char *name) {
    int i;
    i = ps_nlocals - 1;
    while (i >= 0) {
        if (strcmp(name, ps_lname[i]) == 0) return i;
        i = i - 1;
    }
    return -1;
}

static int find_global(char *name) {
    int i;
    i = ps_nglobals - 1;
    while (i >= 0) {
        if (strcmp(name, ps_gname[i]) == 0) return i;
        i = i - 1;
    }
    return -1;
}

/* A static local's object is a global under its mangled name, so an
 * address constant naming one -- static int x; static int *p = &x; --
 * must name that; it named plain `x` and the link failed. */
static char *ps_static_local_sym(char *name) {
    int li;
    if (!ps_in_body) return 0;
    li = find_local(name);
    if (li < 0 || !ps_lstatic[li] || !ps_lsname[li]) return 0;
    return ps_lsname[li];
}

/* An identifier declared again in the scope it was declared in, with
 * no linkage: int a = 1; int a = 2; (6.7p3).  The visible locals of
 * this scope are the last ones with its id. */
static void ps_local_redecl_check(char *name) {
    int j;
    j = ps_nlocals - 1;
    while (j >= 0 && ps_lscope[j] == ps_scope_id) {
        if (strcmp(ps_lname[j], name) == 0) p_error("redefinition of a local");
        j = j - 1;
    }
}

/* A new scope for locals -- a block, a for statement -- and its end;
 * ps_scope_begin returns ps_nlocals and the enclosing scope id is
 * restored from what is passed back (the for statement keeps both in
 * its own locals). */
static int ps_scope_saved[256];
static int ps_scope_sp;
static int ps_scope_begin(void) {
    if (ps_scope_sp < 256) ps_scope_saved[ps_scope_sp] = ps_scope_id;
    ps_scope_sp = ps_scope_sp + 1;
    ps_scope_next = ps_scope_next + 1;
    ps_scope_id = ps_scope_next;
    return ps_nlocals;
}
static void ps_scope_end(int nlocals) {
    ps_nlocals = nlocals;
    ps_scope_sp = ps_scope_sp - 1;
    if (ps_scope_sp < 256) ps_scope_id = ps_scope_saved[ps_scope_sp];
}

static int add_local(char *name, int ty) {
    int idx;
    int sz;
    int al;
    if (ps_nlocals >= P_MAX_LOCALS) {
        p_error("too many locals");
        return 0;
    }
    sz = ty_size(ty);
    if (ty == TY_VOID) p_error("variable has incomplete type 'void'");
    ps_local_redecl_check(name);
    al = ps_local_alignas(ty);
    require_complete_type(ty, "incomplete type for local");
    /* The frame slot is rounded up to a multiple of 4, but the
     * recorded size is the true one: sizeof(local) reads ps_lsize,
     * and a 5-byte struct is 5, not 8. */
    ps_stack = ps_stack + ((sz + 3) / 4) * 4;
    ps_align_stack(al);
    if (ps_stack > ps_stack_max) ps_stack_max = ps_stack;
    idx = ps_nlocals;
    ps_lname[idx] = strdup(name);
    ps_lvolb[idx] = ps_type_volb;
    ps_lvolp[idx] = ps_type_volp;
    ps_lreg[idx] = ps_decl_register;
    ps_lscope[idx] = ps_scope_id;
    ps_loff[idx] = 0 - ps_stack;
    ps_slot_gen = ps_slot_gen + 1;
    ps_lslot[idx] = ps_slot_gen;
    ps_ltype[idx] = ty;
    ps_lcty[idx] = ps_obj_cty(ty);
    ps_larr[idx] = 0;
    ps_lcols[idx] = 0;
    ps_lsub[idx] = 0;
    ps_lsize[idx] = sz;
    ps_lstatic[idx] = 0;
    ps_lsname[idx] = NULL;
    ps_lfpbase[idx] = -1;
    ps_lfpn[idx] = 0;
    if (ps_type_fpbase >= 0) {
        ps_lfpbase[idx] = ps_type_fpbase;
        ps_lfpn[idx] = ps_type_fpn;
    }
    ps_nlocals = ps_nlocals + 1;
    return ps_loff[idx];
}

/* A local-table entry that names a global: a static local (sym is its
 * mangled name) or a block-scope extern (sym is the global's own).  It
 * shadows outer locals like any declaration and allocates no slot.
 * cty is its C type; ps_add_local_alias takes the one parse_type built. */
static int ps_add_local_alias_cty(char *name, int ty, char *sym, int cty) {
    int idx;
    idx = ps_nlocals;
    if (idx >= P_MAX_LOCALS) { p_error("too many locals"); return -1; }
    ps_lname[idx] = strdup(name);
    ps_lvolb[idx] = ps_type_volb;
    ps_lvolp[idx] = ps_type_volp;
    ps_lreg[idx] = 0;
    ps_lscope[idx] = ps_scope_id;
    ps_loff[idx] = 0;
    ps_ltype[idx] = ty;
    ps_lcty[idx] = cty;
    ps_larr[idx] = 0;
    ps_lcols[idx] = 0;
    ps_lsub[idx] = 0;
    ps_lstatic[idx] = 1;
    ps_lsname[idx] = strdup(sym);
    ps_lfpbase[idx] = -1;
    ps_lfpn[idx] = 0;
    if (ps_type_fpbase >= 0) {
        ps_lfpbase[idx] = ps_type_fpbase;
        ps_lfpn[idx] = ps_type_fpn;
    }
    ps_nlocals = ps_nlocals + 1;
    return idx;
}

static int ps_add_local_alias(char *name, int ty, char *sym) {
    return ps_add_local_alias_cty(name, ty, sym, ps_obj_cty(ty));
}

/* A static local array: the alias of global gi, an array of elem_ty
 * (cols: a 2-D array's row width).  Its C type is the global's, taken
 * now that an initializer has fixed the size. */
static int ps_add_static_array_alias(char *name, int elem_ty, int cols, int gi) {
    int idx;
    idx = ps_add_local_alias_cty(name, elem_ty + TY_PTR, ps_sl_buf,
                                 ps_global_cty(elem_ty + TY_PTR, ps_gsize[gi]));
    if (idx < 0) return -1;
    ps_larr[idx] = 1;
    ps_lcols[idx] = cols;
    ps_lsub[idx] = cols > 0 ? ps_gsub[gi] : 0;
    ps_local_rows_cty(idx);     /* its initializer is data, already written */
    return idx;
}

/* A block-scope static array of function pointers, static int
 * (*slot[2])(long long): its C type an array of them, from the return
 * type in ps_type_cty and the declarator's stars -- it had int
 * elements, and slot[1] = sink stored a pointer into an int. */
static int ps_add_static_fnptr_alias(char *name, int nstars, int gi) {
    ps_fnptr_nstar = nstars;
    ps_type_cty = ct_array(ps_fnptr_cty(ps_type_cty), -1);
    return ps_add_static_array_alias(name, TY_INT, 0, gi);
}

/* Stamp a VAR node with the live local's slot_id (GitHub issue 77). */
static void ps_bind_var(Node *n, int li) {
    n->cty = ps_lcty[li];
    n->is_local = 1;
    n->slot_id = ps_lslot[li];
    n->is_array = ps_larr[li];
    n->arr_cols = ps_lcols[li];
    n->arr_sub = ps_lsub[li];
}

/* The C type of T (*name)[cols]: a pointer to an array of cols T.  The
 * element is built from its machine type: what parse_type left in
 * ps_type_cty is the declarator's function-pointer type by now, and it
 * "agrees" with int. */
static int ps_rowptr_cty(int elem_ty, int cols) {
    return ct_ptr(ct_array(ct_from_mty(elem_ty, 0), cols));
}

/* A local T (*name)[N] = ...;, the cursor on the `[`: a pointer to an
 * array of N, its machine type T * with the row width beside it, as a
 * parameter of that type has.  It was refused. */
static Node *ps_local_rowptr(char *nm, int ty, int nstars, int is_static) {
    int cols;
    int off;
    int li;
    Node *n;
    if (ps_vla_ahead()) return ps_local_vm_ptr(nm, ty, nstars, is_static);
    cols = ps_inner_dims(0, &ps_dims_sub);     /* T (*name)[M][K]: rows of rows */
    if (nstars != 1 || is_static) {
        p_error("pointer to array: only a local T (*name)[N] is supported");
        return nd_num(0);
    }
    off = add_local(nm, ty + TY_PTR);
    li = ps_nlocals - 1;
    ps_lcols[li] = cols;
    ps_lsub[li] = ps_dims_sub;
    ps_lcty[li] = ct_ptr(ps_rows_cty(ct_from_mty(ty, 0), cols, ps_dims_sub));
    if (lex_tok != TK_ASSIGN) {
        expect(TK_SEMI);
        return nd_block(NULL);
    }
    next();
    n = nd_var(nm, off, ty + TY_PTR);
    ps_bind_var(n, li);
    n = nd_assign(n, parse_assign());
    expect(TK_SEMI);
    return nd_expr_stmt(n);
}

/* Add a local array. Returns offset of start of array. */
/* A local's _Alignas: the alignment its slot needs beyond the frame's
 * word, or 0.  A stronger one than 4 has the function's prologue
 * realign sp (hir_codegen.h; the function is not inlined) and the slot
 * placed at a multiple of it from the frame's top (ps_align_stack). */
static int ps_local_alignas(int elem_ty) {
    int a;
    if (ps_type_alignas == 0) return 0;
    a = ps_alignas_for(elem_ty);
    if (a <= 4) return 0;
    if (a > 2048) p_error("_Alignas on a local is limited to 2048");
    if (ps_is_varargs) p_error("_Alignas stronger than 4 on a local of a variadic function is not supported");
    if (a > ps_fn_align) ps_fn_align = a;
    return a;
}

/* The frame's top is aligned to ps_fn_align: a slot ending ps_stack
 * bytes below it is aligned to a when ps_stack is a multiple of a. */
static void ps_align_stack(int a) {
    if (a > 4) ps_stack = ((ps_stack + a - 1) / a) * a;
}

static int add_local_array(char *name, int elem_ty, int count) {
    int al;
    int idx;
    int total;
    int elem_sz;
    if (ps_nlocals >= P_MAX_LOCALS) {
        p_error("too many locals");
        return 0;
    }
    al = ps_local_alignas(elem_ty);
    elem_sz = ty_size(elem_ty);
    require_complete_type(elem_ty, "incomplete element type");
    total = elem_sz * count;
    /* Frame slot rounded to 4; ps_lsize keeps the true byte count so
     * sizeof(char x[] = {5 bytes}) is 5, not 8 (libutf's harness passed
     * the padded size into co_right/co_reverse and both misbehaved). */
    ps_stack = ps_stack + ((total + 3) / 4) * 4;
    ps_align_stack(al);
    if (ps_stack > ps_stack_max) ps_stack_max = ps_stack;
    idx = ps_nlocals;
    ps_lname[idx] = strdup(name);
    ps_lvolb[idx] = ps_type_volb;
    ps_lvolp[idx] = ps_type_volp;
    ps_lreg[idx] = ps_decl_register;
    ps_lscope[idx] = ps_scope_id;
    ps_loff[idx] = 0 - ps_stack;
    ps_slot_gen = ps_slot_gen + 1;
    ps_lslot[idx] = ps_slot_gen;
    ps_ltype[idx] = elem_ty + TY_PTR;  /* array decays to pointer */
    ps_lcty[idx] = ct_array(ps_obj_cty(elem_ty), count);
    ps_larr[idx] = 1;
    ps_lcols[idx] = 0;
    ps_lsub[idx] = 0;
    ps_lsize[idx] = total;
    /* The table is reused across functions: without this clear, a
     * local array landing on a slot a previous function used for a
     * STATIC local inherits ps_lstatic/ps_lsname and silently aliases
     * that function's mangled global (doom: D_BindVariables' buf
     * became D_Display.menuactivestate.1). */
    ps_lstatic[idx] = 0;
    ps_lsname[idx] = NULL;
    ps_lfpbase[idx] = -1;
    ps_lfpn[idx] = 0;
    /* An array of function pointers keeps the element signature, the
     * same way add_local/add_global do -- without it a LOCAL `dfn t[2]`
     * lost what the global `dfn gtab[2]` kept, and t[0](x) fell back to
     * the TY_INT default (GitHub issue 70). */
    if (ps_type_fpbase >= 0) {
        ps_lfpbase[idx] = ps_type_fpbase;
        ps_lfpn[idx] = ps_type_fpn;
    }
    ps_nlocals = ps_nlocals + 1;
    return ps_loff[idx];
}

/* An array of elem: count elements (-1 unknown); with cols, count is the
 * flattened total of an array of rows of cols, themselves shaped by sub
 * past two dimensions. */
static int ps_array_cty(int elem, int count, int cols, int sub) {
    if (cols > 0)
        return ct_array(ps_rows_cty(elem, cols, sub), count > 0 ? count / cols : -1);
    return ct_array(elem, count > 0 ? count : -1);
}

/* An initializer fixed global idx's length: its C type takes it. */
static void ps_fix_global_array(int idx, int count, int cols) {
    int elem;
    elem = ps_gcty[idx];
    if (!ct_is_array(elem)) return;
    while (ct_is_array(elem)) elem = ct_baseof(elem);
    ps_gcty[idx] = ps_array_cty(elem, count, cols, ps_gsub[idx]);
}

/* Local li, a 2-D array with rows of ps_lcols[li]: give its C type the
 * rows.  It is registered flat, and stays flat while the parser writes
 * its initializer -- those stores step through it element by element --
 * but user code sees the rows: *m on int m[3][4] is a row of four, and
 * m + 1 steps over one (it was an int, and 4 bytes). */
static void ps_local_rows_cty(int li) {
    int elem;
    int count;
    elem = ps_lcty[li];
    if (ps_lcols[li] <= 0 || elem <= 0 || !ct_is_array(elem) ||
        ct_is_array(ct_baseof(elem))) return;
    count = ct_countof(elem);
    ps_lcty[li] = ps_array_cty(ct_baseof(elem), count, ps_lcols[li], ps_lsub[li]);
}

/* A global's C type: an array when the parser records one (a pointer
 * machine type with a size -- the element pointer of a decayed array). */
static int ps_global_cty(int ty, int size_bytes) {
    int esz;
    int elem;
    /* size 0: an initializer is still to set the length (ps_fix_global_array) */
    if (ty_is_ptr(ty) && (size_bytes > 0 || (ps_type_cty > 0 && ct_is_array(ps_type_cty)))) {
        esz = ty_size(ty_deref(ty));
        /* the declaration's C type is the array, or its pointer form:
         * either way its base is the element -- static H tab[4], H a
         * function-pointer typedef, had int elements */
        if (ps_type_cty > 0 && (ct_is_ptr(ps_type_cty) || ct_is_array(ps_type_cty)) &&
            ct_mty_agrees(ct_baseof(ps_type_cty), ty_deref(ty)))
            elem = ct_baseof(ps_type_cty);
        else
            elem = ps_obj_cty(ty_deref(ty));
        return ct_array(elem, esz > 0 ? size_bytes / esz : -1);
    }
    return ps_obj_cty(ty);
}

static int add_global(char *name, int ty, int size_bytes) {
    int idx;
    if (ps_nglobals >= P_MAX_GLOBALS) {
        p_error("too many globals");
        return 0;
    }
    idx = ps_nglobals;
    ps_gname[idx] = strdup(name);
    ps_gconst[idx] = ps_decl_objconst;
    ps_decl_objconst = 0;
    ps_galign[idx] = 0;
    ps_global_alignas(idx, ty, size_bytes);
    ps_gcty[idx] = ps_global_cty(ty, size_bytes);
    ps_gvolb[idx] = ps_type_volb;
    ps_gvolp[idx] = ps_type_volp;
    ps_gtype[idx] = ty;
    ps_gsize[idx] = size_bytes;
    ps_ginit[idx] = 0;
    ps_ginit_hi[idx] = 0;
    ps_gstr[idx] = -1;
    ps_ginit_start[idx] = -1;
    ps_ginit_count[idx] = 0;
    ps_girel_start[idx] = -1;
    ps_girel_count[idx] = 0;
    ps_glocal[idx] = 0;
    ps_gextern[idx] = 0;
    ps_gcols[idx] = 0;
    ps_gsub[idx] = 0;
    ps_ghasinit[idx] = 0;
    ps_gfpbase[idx] = -1;
    ps_gfpn[idx] = 0;
    if (ps_type_fpbase >= 0) {
        ps_gfpbase[idx] = ps_type_fpbase;
        ps_gfpn[idx] = ps_type_fpn;
    }
    ps_nglobals = ps_nglobals + 1;
    return idx;
}

static void ps_reset_global_init(int idx) {
    ps_ginit[idx] = 0;
    ps_ginit_hi[idx] = 0;
    ps_gstr[idx] = -1;
    ps_ginit_start[idx] = -1;
    ps_ginit_count[idx] = 0;
    ps_girel_start[idx] = -1;
    ps_girel_count[idx] = 0;
    ps_ghasinit[idx] = 0;
}

/* Record that this object now has a completed initializer.  A second
 * initializer is a redefinition (int x = 1; int x = 2;).  int x = 0
 * must count: ps_ginit==0 is also the tentative-definition default. */
static void ps_mark_global_init(int idx) {
    if (ps_ghasinit[idx]) p_error("redefinition of global");
    ps_ghasinit[idx] = 1;
}

static void ps_set_scalar_init(int idx, int lo, int hi) {
    /* a _Bool object holds 0 or 1 (6.3.1.2): static _Bool s = 5 is 1 */
    if (ps_gcty[idx] > 0 && ct_kindof(ct_unqual(ps_gcty[idx])) == CT_BOOL) {
        lo = (lo != 0 || hi != 0);
        hi = 0;
    }
    ps_mark_global_init(idx);
    ps_ginit[idx] = lo;
    ps_ginit_hi[idx] = hi;
}

static void ps_set_str_init(int idx, int sp) {
    ps_mark_global_init(idx);
    ps_gstr[idx] = sp;
}

/* What an object's declaration may not be (6.7.4p3, 6.7p7): inline,
 * or of type void. */
static void ps_object_decl_check(int ty) {
    if (ps_top_inline) {
        ps_top_inline = 0;
        p_error("'inline' can only appear on functions");
    }
    if (ty == TY_VOID) p_error("variable has incomplete type 'void'");
}

/* The innermost element of an array type -- or of an array held in
 * pointer form, as a global declared before its size is known is. */
static int ps_array_elem(int t, int is_arr) {
    t = ct_unqual(t);
    if (!ct_is_array(t) && ct_is_ptr(t) && is_arr) t = ct_baseof(t);
    while (ct_is_array(t)) t = ct_baseof(t);
    return t;
}

/* Two declarations of one global, compatible (6.7p4).  Where either is
 * an array, the elements are compared: an array declared before its
 * size is known is held in pointer form, and a 2-D one may be held flat
 * or by rows -- extern int a[]; extern int a[3]; is one array, and so
 * is DOOM's scalelight[LIGHTLEVELS][MAXLIGHTSCALE] in r_state.h and
 * r_main.c. */
static int ps_global_types_agree(int a, int b) {
    int arr;
    arr = ct_is_array(a) || ct_is_array(b);
    if (!arr) return sema_compat(ct_unqual(a), ct_unqual(b));
    return sema_compat(ps_array_elem(a, 1), ps_array_elem(b, 1));
}

/* The size an array's declaration wrote, kept for the next to agree
 * with: the first one written, in bytes.  An array declared without
 * one is given an element at once (ps_tentative_count), and says so in
 * ps_decl_unsized, so its 1 is not taken for a written [1]. */
static int ps_gdeclsz[P_MAX_GLOBALS];
static int ps_decl_unsized;

static void ps_note_declsz(int idx, int ty, int size_bytes) {
    if (!ps_decl_unsized && ty_is_ptr(ty) && size_bytes > 0 && ps_gdeclsz[idx] == 0)
        ps_gdeclsz[idx] = size_bytes;
    ps_decl_unsized = 0;
}

/* A redeclaration of global idx as type ty (size bytes): compatible
 * with what was declared (6.7p4), and not static after a declaration
 * with external linkage (6.2.2p7). */
static void ps_global_redecl_check(int idx, int ty, int size_bytes) {
    int t;
    t = ps_global_cty(ty, size_bytes);
    if (ps_gcty[idx] > 0 && t > 0 && !ps_global_types_agree(ps_gcty[idx], t))
        p_error("redeclaration with a different type");
    /* an array's two sizes, where both are written, are one size
     * (6.7.6.2p6), counted in bytes: rows or flat alike */
    if (!ps_decl_unsized && ty_is_ptr(ty) && size_bytes > 0 && ps_gdeclsz[idx] > 0 &&
        ps_gdeclsz[idx] != size_bytes)
        p_error("redeclaration with a different type");
    if (ps_top_static && !ps_glocal[idx])
        p_error("static declaration follows non-static declaration");
}

/* A global's _Alignas, onto global idx (6.7.5): the element's
 * alignment for an array, which is held in pointer form. */
static void ps_global_alignas(int idx, int ty, int size_bytes) {
    int a;
    if (ps_type_alignas == 0) return;
    a = ps_alignas_for((size_bytes > 0 && ty_is_ptr(ty)) ? ty_deref(ty) : ty);
    if (a > ps_galign[idx]) ps_galign[idx] = a;
}

static int add_extern_global(char *name, int ty, int size_bytes) {
    int idx;
    ps_top_inline = 0;      /* extern void v; is a declaration, not an object */
    idx = find_global(name);
    if (idx >= 0) ps_global_redecl_check(idx, ty, size_bytes);
    ps_top_static = 0;
    if (idx >= 0) {
        ps_decl_objconst = 0;
        ps_note_declsz(idx, ty, size_bytes);
        return idx;
    }
    idx = add_global(name, ty, size_bytes);
    ps_gextern[idx] = 1;
    ps_note_declsz(idx, ty, size_bytes);
    return idx;
}

static int add_defined_global(char *name, int ty, int size_bytes) {
    int idx;
    ps_object_decl_check(ty);
    idx = find_global(name);
    if (idx >= 0 && ps_block_depth == 0) ps_global_redecl_check(idx, ty, size_bytes);
    ps_top_static = 0;
    if (idx >= 0) {
        /* An extern declaration being defined, or a tentative definition
         * (`u32 sqlite3WhereTrace;`) followed by the real one
         * (`u32 sqlite3WhereTrace = 0;`): one object, one label.  Keeping
         * both emitted the label twice.
         *
         * The other legal C order is a definition then a tentative
         * (`int x = 7; int x;`): keep the initializer.  Unconditionally
         * resetting here zeroed it (GitHub issue 46).  Two initializers
         * are a redefinition; ps_mark_global_init rejects that. */
        ps_gtype[idx] = ty;
        ps_gcty[idx] = ps_global_cty(ty, size_bytes);
        ps_gsize[idx] = size_bytes;
        ps_glocal[idx] = 0;
        ps_gextern[idx] = 0;
        ps_gconst[idx] = ps_decl_objconst;   /* the definition decides */
        ps_decl_objconst = 0;
        ps_global_alignas(idx, ty, size_bytes);
        if (!ps_ghasinit[idx])
            ps_reset_global_init(idx);
        ps_note_declsz(idx, ty, size_bytes);
        return idx;
    }
    idx = add_global(name, ty, size_bytes);
    ps_note_declsz(idx, ty, size_bytes);
    return idx;
}

/* --- Variable length arrays (6.7.6.2) ---
 * A VLA's C type is an array whose count is -2 - id, id naming the
 * hidden local that holds the count (ps_vc_var), evaluated where the
 * declarator is.  The object is a hidden pointer: sp is moved down by
 * its size and the pointer kept, and the C type tells the lowering both
 * that (its address and its value are the pointer, hir_lower.h
 * hl_is_vla_obj) and how far a step through it goes (a pointer to a VLA
 * scales at run time, hl_scale).  The sp from before it is kept too;
 * leaving its scope -- a block's end, a break, continue or goto out of
 * it -- sets sp back (ps_vla_restore), and a return pops the frame.
 * SLOW-32 only: the cross targets keep __STDC_NO_VLA__. */
#define PS_MAX_VC 8192
static Node *ps_vc_var[PS_MAX_VC];     /* NULL for [*], or a prototype's count */
static int   ps_nvc;
#define PS_MAX_VPEND 64
static Node *ps_vpend[PS_MAX_VPEND];   /* count assignments read, not yet placed */
static int   ps_nvpend;
static Node *ps_vpp_expr[PS_MAX_VPEND]; /* a parameter's counts, for the body's entry */
static int   ps_vpp_id[PS_MAX_VPEND];
static int   ps_nvpp;
#define PS_MAX_VGOTO 1024
static Node *ps_vgoto[PS_MAX_VGOTO];
static int   ps_nvgoto;
static int   ps_vla_names;

static void ps_vla_target_check(void) {
#ifdef S12CC_TARGET_X64
    p_error("variable length arrays are not supported on this target");
#endif
#ifdef S12CC_TARGET_A64
    p_error("variable length arrays are not supported on this target");
#endif
}

/* A fresh reference to local v (a node is not shared between uses). */
static Node *ps_var_copy(Node *v) {
    Node *n;
    n = nd_var(v->name, v->offset, v->ty);
    n->is_local = 1;
    n->slot_id = v->slot_id;
    n->cty = v->cty;
    return n;
}

/* A hidden int local, __vla.N: a count, or a saved sp. */
static Node *ps_vla_hidden(void) {
    char nm[24];
    int v;
    int i;
    int d;
    int sv_al;
    int sv_reg;
    int li;
    Node *n;
    memcpy(nm, "__vla.", 6);
    i = 6;
    v = ps_vla_names;
    ps_vla_names = ps_vla_names + 1;
    d = 1;
    while (d * 10 <= v) d = d * 10;
    while (d > 0) {
        nm[i] = (char)(48 + (v / d) % 10);
        i = i + 1;
        d = d / 10;
    }
    nm[i] = 0;
    sv_al = ps_type_alignas;
    sv_reg = ps_decl_register;
    ps_type_alignas = 0;
    ps_decl_register = 0;
    add_local(nm, TY_INT);
    ps_type_alignas = sv_al;
    ps_decl_register = sv_reg;
    li = ps_nlocals - 1;
    ps_lcty[li] = ct_basic[CT_INT];
    n = nd_var(nm, ps_loff[li], TY_INT);
    ps_bind_var(n, li);
    return n;
}

/* Does a declarator's dimension list, at the cursor, hold one that is
 * not an integer constant expression -- or [*]?  Read ahead and put
 * back; `[ number ]` and `[]` are taken without parsing. */
static int ps_vla_ahead(void) {
    int vm;
    Node *e;
    if (lex_tok != TK_LBRACK) return 0;
    vm = 0;
    ps_lex_mark();
    while (lex_tok == TK_LBRACK && !vm) {
        next();
        while (lex_tok == TK_CONST || lex_tok == TK_VOLATILE || lex_tok == TK_RESTRICT ||
               lex_tok == TK_STATIC || is_gnu_qual_ident())
            next();
        if (lex_tok == TK_NUM) {
            next();
            if (lex_tok == TK_RBRACK) { next(); continue; }
        }
        if (lex_tok == TK_STAR) {
            next();
            if (lex_tok == TK_RBRACK) { vm = 1; break; }
        }
        if (lex_tok != TK_RBRACK) {
            e = parse_assign();
            sema_expr(e);
            if (!(e->attr & AT_ICX)) vm = 1;
        }
        if (lex_tok != TK_RBRACK) break;
        next();
    }
    ps_lex_rewind();
    return vm;
}

/* A dimension's count that is not a constant: an id for its C type, and
 * the assignment to its hidden local -- pending, for the declaration to
 * place, or for a parameter's at the body's entry (6.9.1p10). */
static int ps_vla_count(Node *e, int param) {
    int id;
    Node *v;
    if (ps_nvc >= PS_MAX_VC || ps_nvpend >= PS_MAX_VPEND || ps_nvpp >= PS_MAX_VPEND)
        p_error("too many variable length array dimensions");
    id = ps_nvc;
    ps_nvc = ps_nvc + 1;
    ps_vc_var[id] = NULL;
    if (param) {
        ps_vpp_expr[ps_nvpp] = e;
        ps_vpp_id[ps_nvpp] = id;
        ps_nvpp = ps_nvpp + 1;
        return id;
    }
    v = ps_vla_hidden();
    ps_vc_var[id] = v;
    ps_vpend[ps_nvpend] = nd_assign(ps_var_copy(v), e);
    ps_nvpend = ps_nvpend + 1;
    return id;
}

/* The dimensions at the cursor, over element type elem: the C type.  A
 * parameter's first is adjusted away and not kept; [*] is unspecified. */
static int ps_vla_dims(int elem, int param) {
    int cnt[16];
    int nd;
    int c;
    Node *e;
    nd = 0;
    while (lex_tok == TK_LBRACK) {
        next();
        while (lex_tok == TK_CONST || lex_tok == TK_VOLATILE || lex_tok == TK_RESTRICT ||
               lex_tok == TK_STATIC || is_gnu_qual_ident())
            next();
        c = -1;
        if (lex_tok == TK_STAR) {
            ps_lex_mark();
            next();
            if (lex_tok == TK_RBRACK) ps_lex_commit();
            else ps_lex_rewind();
        }
        if (lex_tok != TK_RBRACK) {
            e = parse_assign();
            sema_expr(e);
            if (e->attr & AT_ICX) {
                c = (int)sema_cvalof(e);
                if (c < 0) p_error("array has a negative size");
            } else if (!(param && nd == 0)) {
                c = -2 - ps_vla_count(e, param);
            }
        }
        expect(TK_RBRACK);
        if (nd >= 16) p_error("too many array dimensions");
        cnt[nd] = c;
        nd = nd + 1;
    }
    while (nd > 0) {
        nd = nd - 1;
        elem = ct_array(elem, cnt[nd]);
    }
    return elem;
}

/* The size of type t, a Node: a product of counts when it is known only
 * at run time. */
static Node *ps_vla_size(int t) {
    Node *n;
    Node *c;
    int k;
    if (!ct_runtime_size(t)) return nd_size(ct_sizeof(t));
    k = ct_countof(t);
    if (k >= 0) {
        c = nd_size(k);
    } else {
        if (k == -1 || ps_vc_var[-2 - k] == NULL)
            p_error("the size of an array of unspecified size is needed");
        c = ps_var_copy(ps_vc_var[-2 - k]);
    }
    n = nd_binop(TK_STAR, c, ps_vla_size(ct_baseof(t)));
    n->ty = TY_INT | TY_UNSIGNED;
    return n;
}

/* The pending count assignments, as statements before tail ... */
static Node *ps_vla_pend_stmts(Node *tail) {
    Node *head;
    Node *s;
    int i;
    head = tail;
    i = ps_nvpend;
    while (i > 0) {
        i = i - 1;
        s = nd_expr_stmt(ps_vpend[i]);
        s->next = head;
        head = s;
    }
    ps_nvpend = 0;
    return head;
}

/* ... or evaluated before expression e. */
static Node *ps_vla_pend_comma(Node *e) {
    int i;
    i = ps_nvpend;
    while (i > 0) {
        i = i - 1;
        e = nd_comma(ps_vpend[i], e);
    }
    ps_nvpend = 0;
    return e;
}

static Node *ps_vla_asm(int kind, Node *arg) {
    return nd_asm("", NULL, arg, kind, arg != NULL);
}

/* sp set back to what it was before VLA object id. */
static Node *ps_vla_restore(int id) {
    return nd_expr_stmt(ps_vla_asm(ASM_S32_SETSP, ps_var_copy(ps_vd_save[id])));
}

/* The first VLA object, on top's chain, inside mark: the one whose sp a
 * jump from top to mark restores (-1: mark is not on the chain). */
static int ps_vla_first(int top, int mark) {
    while (top >= 0 && ps_vd_parent[top] != mark) top = ps_vd_parent[top];
    return top;
}

/* A block-scope declaration of a VLA object, the cursor after its name:
 * dimensions to read, or (elem a VLA type already) none. */
static Node *ps_local_vla(char *nm, int elem) {
    int cty;
    int inner;
    int pty;
    int li;
    Node *v;
    Node *save;
    Node *s;
    Node *a;
    ps_vla_target_check();
    if (ps_type_alignas) p_error("_Alignas on a variable length array is not supported");
    cty = elem;
    if (lex_tok == TK_LBRACK) cty = ps_vla_dims(elem, 0);
    if (lex_tok == TK_ASSIGN) p_error("variable-sized object may not be initialized");   /* 6.7.9p3 */
    inner = cty;
    while (ct_kindof(inner) == CT_ARRAY) inner = ct_baseof(inner);
    pty = ct_mty(inner) + TY_PTR;
    save = ps_vla_hidden();
    add_local(nm, pty);
    li = ps_nlocals - 1;
    ps_lcty[li] = cty;
    v = nd_var(nm, ps_loff[li], pty);
    ps_bind_var(v, li);
    v->cty = ct_ptr(inner);         /* the slot itself, a pointer: stored to */
    s = nd_expr_stmt(nd_assign(ps_var_copy(save), ps_vla_asm(ASM_S32_GETSP, NULL)));
    a = ps_vla_asm(ASM_S32_VLA_ALLOC, ps_vla_size(cty));
    a->ty = pty;
    a->cty = ct_ptr(inner);
    s->next = nd_expr_stmt(nd_assign(v, a));
    if (ps_nvd >= PS_MAX_VD) p_error("too many variable length arrays");
    ps_vd_parent[ps_nvd] = ps_vd_top;
    ps_vd_save[ps_nvd] = save;
    ps_vd_top = ps_nvd;
    ps_nvd = ps_nvd + 1;
    ps_fn_vm = 1;
    return nd_block(ps_vla_pend_stmts(s));
}

/* Is the declarator at the cursor a VLA object's: a dimension not
 * constant, or an object of a VLA type (a VLA typedef's)? */
static int ps_vla_object_ahead(void) {
    if (ps_decl_inner > 0) return 0;
    if (ps_type_cty > 0 && ct_runtime_size(ps_type_cty)) return 1;
    return ps_vla_ahead();
}

/* A block-scope T (*name)[n]: a pointer to a VLA, the cursor on `[`. */
static Node *ps_local_vm_ptr(char *nm, int ty, int nstars, int is_static) {
    int cty;
    int i;
    int li;
    Node *n;
    Node *init;
    ps_vla_target_check();
    if (is_static) p_error("an object of static storage duration may not have a variably modified type");
    cty = ps_vla_dims(ct_from_mty(ty, 0), 0);
    i = 0;
    while (i < nstars) {
        cty = ct_ptr(cty);
        ty = ty + TY_PTR;
        i = i + 1;
    }
    add_local(nm, ty);
    li = ps_nlocals - 1;
    ps_lcty[li] = cty;
    ps_fn_vm = 1;
    init = NULL;
    if (lex_tok == TK_ASSIGN) {
        next();
        n = nd_var(nm, ps_loff[li], ty);
        ps_bind_var(n, li);
        init = nd_expr_stmt(nd_assign(n, parse_assign()));
    }
    expect(TK_SEMI);
    return nd_block(ps_vla_pend_stmts(init));
}

/* typedef T name[n]; in a block, the cursor on `[`: the size evaluated
 * here, once (6.7.6.2p5); the statements are pending for parse_stmt. */
static void ps_vla_typedef(char *nm, int cty) {
    int inner;
    ps_vla_target_check();
    if (ps_block_depth == 0) p_error("variably modified type at file scope");   /* 6.7.6.2p2 */
    cty = ps_vla_dims(cty, 0);
    inner = cty;
    while (ct_kindof(inner) == CT_ARRAY) inner = ct_baseof(inner);
    add_typedef(nm, ct_mty(inner) + TY_PTR);
    ps_tdcty[ps_ntypedefs - 1] = cty;
    ps_tdarr[ps_ntypedefs - 1] = 0;
    ps_typedef_check();
    ps_fn_vm = 1;
    skip_gnu_decl_suffixes();
    expect(TK_SEMI);
}

/* sizeof of a VLA type at the cursor -- T[n] or a VLA typedef -- after
 * its base type: the counts evaluated, then the size (6.5.3.4p2). */
static int ps_vla_sizeof_ahead(void) {
    if (lex_tok == TK_RPAREN) return ps_type_cty > 0 && ct_runtime_size(ps_type_cty);
    return ps_vla_ahead();
}

static Node *ps_vla_sizeof_type(void) {
    int cty;
    ps_vla_target_check();
    if (ps_block_depth == 0) p_error("variably modified type at file scope");
    cty = ps_type_cty;
    if (lex_tok == TK_LBRACK) cty = ps_vla_dims(cty, 0);
    expect(TK_RPAREN);
    return ps_vla_pend_comma(ps_vla_size(cty));
}

/* sizeof expression: a VLA's size at run time, its operand evaluated
 * (6.5.3.4p2); any other's, the constant. */
static int ps_sizeof_node(Node *n);
static Node *ps_sizeof_value(Node *n) {
    sema_expr(n);
    if (n->cty > 0 && ct_runtime_size(n->cty)) return nd_comma(n, ps_vla_size(n->cty));
    return nd_size(ps_sizeof_node(n));
}

/* p[i] where p points to a VLA -- int g[n][m], int (*p)[m] -- is a row,
 * an array: its value is its address, as p's machine type has it, and
 * its C type the row's.  The parser needs that before sema does: a row
 * of structures is subscripted and then a member taken. */
static void ps_vla_row(Node *n) {
    Node *p;
    int t;
    p = n->lhs->lhs;
    t = p->cty > 0 ? ct_decay(p->cty) : 0;
    if (t > 0 && ct_is_ptr(t) && ct_runtime_size(ct_baseof(t))) {
        n->ty = p->ty;
        n->cty = ct_baseof(t);
    }
}

/* A parameter's dimensions, when one is not constant: a pointer to the
 * element (6.7.6.3p7) whose type is variably modified -- int a[n][m] is
 * int (*a)[m], m's count taken at the body's entry. */
static int ps_vla_param_dims(int *pty) {
    int cty;
    ps_vla_target_check();
    cty = ps_vla_dims(ps_type_cty, 1);
    *pty = *pty + TY_PTR;
    ps_type_cty = ct_ptr(ct_baseof(cty));
    ps_fn_vm = 1;
    return 0;
}

/* At a definition's body: the parameters' counts, into hidden locals,
 * the statements before pre. */
static Node *ps_vla_param_pre(Node *pre) {
    int k;
    Node *v;
    Node *s;
    Node *head;
    Node *tail;
    head = NULL;
    tail = NULL;
    k = 0;
    while (k < ps_nvpp) {
        v = ps_vla_hidden();
        ps_vc_var[ps_vpp_id[k]] = v;
        s = nd_expr_stmt(nd_assign(ps_var_copy(v), ps_vpp_expr[k]));
        if (head == NULL) head = s;
        else tail->next = s;
        tail = s;
        k = k + 1;
    }
    ps_nvpp = 0;
    if (head == NULL) return pre;
    tail->next = pre;
    return head;
}

/* break, continue, goto, case and default keep the VLA object innermost
 * in scope where they are (offset, + 1: 0 is none); a goto is listed
 * for the check at the function's end. */
static Node *ps_vla_jump(Node *n) {
    n->offset = ps_vd_top + 1;
    if (n->kind == ND_GOTO && ps_nvgoto < PS_MAX_VGOTO) {
        ps_vgoto[ps_nvgoto] = n;
        ps_nvgoto = ps_nvgoto + 1;
    }
    return n;
}

/* Jump s leaving VLA objects for mark: { sp set back; s }, s mutated in
 * place so its parent keeps it. */
static void ps_vla_wrap(Node *s, int mark) {
    int first;
    Node *j;
    Node *r;
    first = ps_vla_first(s->offset - 1, mark);
    if (s->offset - 1 == mark || first < 0) return;
    j = nd_new(s->kind);
    j->val = s->val;
    j->offset = mark + 1;
    j->line = s->line;
    j->file = s->file;
    r = ps_vla_restore(first);
    r->next = j;
    s->kind = ND_BLOCK;
    s->body = r;
}

/* The jumps out of a loop or switch body: break (brk), continue (cont);
 * a switch's case labels may not be inside a VLA's scope (cases). */
static void ps_vla_fix(Node *s, int mark, int brk, int cont, int cases) {
    int k;
    while (s) {
        k = s->kind;
        if (k == ND_BREAK && brk) ps_vla_wrap(s, mark);
        else if (k == ND_CONTINUE && cont) ps_vla_wrap(s, mark);
        else if ((k == ND_CASE || k == ND_DEFAULT) && cases) {
            if (s->offset - 1 != mark)
                p_error("switch jumps into the scope of a variably modified object");   /* 6.8.4.2p2 */
        } else if (k == ND_SWITCH) {
            if (cont) ps_vla_fix(s->body, mark, 0, 1, 0);
        } else if (k == ND_BLOCK || k == ND_LABEL || k == ND_IF) {
            ps_vla_fix(s->body, mark, brk, cont, cases);
            if (k == ND_IF) ps_vla_fix(s->els, mark, brk, cont, cases);
        }
        s = s->next;
    }
}

static Node *ps_vla_loop(Node *n, int is_loop) {
    if (ps_nvd > 0) ps_vla_fix(n->body, ps_vd_top, 1, is_loop, !is_loop);
    return n;
}

/* At a block's end: sp set back for its VLA objects (appended to the
 * statements head..tail). */
static Node *ps_vla_block_end(Node *head, Node *tail, int vtop) {
    Node *r;
    if (ps_vd_top == vtop) return head;
    r = ps_vla_restore(ps_vla_first(ps_vd_top, vtop));
    ps_vd_top = vtop;
    if (head == NULL) return r;
    tail->next = r;
    return head;
}

/* At a function's end: each goto into a VLA's scope refused (6.8.6.1p1),
 * each out of one given its restore. */
static void ps_vla_gotos(void) {
    int g;
    int i;
    int lt;
    int t;
    Node *n;
    if (ps_nvd == 0) return;
    g = 0;
    while (g < ps_nvgoto) {
        n = ps_vgoto[g];
        i = 0;
        while (i < ps_nlabels && ps_lblid[i] != n->val) i = i + 1;
        if (i < ps_nlabels && ps_lbldef[i]) {
            lt = ps_lblvtop[i];
            t = n->offset - 1;
            while (t >= 0 && t != lt) t = ps_vd_parent[t];
            if (t != lt) p_error("goto jumps into the scope of a variably modified object");
            ps_vla_wrap(n, lt);
        }
        g = g + 1;
    }
}

/* Array dimensions on a parameter.  `T a[N]` is `T *a`; `T a[N][M]` is a
 * pointer to a row of M elements, ONE pointer level with the row width
 * recorded so a[i] scales by the row and stays a row pointer (the
 * same shape as a pointer-to-array struct member).  It used to add a
 * pointer level per `[`, making `char fields[4][16]` a `char **`, and
 * regal's CSV callback passed the first row's bytes to strcasecmp as
 * a pointer.  Returns the row width (0 when not 2D). */
static int ps_parse_param_dims(int *pty) {
    int cols;
    ps_dims_sub = 0;
    if (ps_vla_ahead()) return ps_vla_param_dims(pty);
    if (lex_tok != TK_LBRACK) return 0;
    next();
    /* a[restrict static 1], a[const]: the qualifiers of the pointer
     * the parameter becomes, and static's promise of at least that
     * many elements (6.7.6.3p7) -- nothing to emit */
    while (lex_tok == TK_CONST || lex_tok == TK_VOLATILE || lex_tok == TK_RESTRICT ||
           lex_tok == TK_STATIC || is_gnu_qual_ident())
        next();
    if (lex_tok != TK_RBRACK) parse_const_int();
    expect(TK_RBRACK);
    /* the dimensions after the first make the row it points to, with
     * the strides below in ps_dims_sub past two dimensions */
    cols = ps_inner_dims(0, &ps_dims_sub);
    *pty = *pty + TY_PTR;
    /* the C type too: a parameter declared as an array is a pointer to
     * its element, a row for T a[][M] (6.7.6.3p7) */
    if (cols > 0) ps_type_cty = ct_ptr(ps_rows_cty(ps_type_cty, cols, ps_dims_sub));
    else ps_type_cty = ct_ptr(ps_type_cty);
    return cols;
}

/* Build mangled name for a static local: funcname.varname.N */
static void ps_mangle_static(char *func, char *var) {
    int i;
    int j;
    int v;
    int d;
    char digits[12];
    i = 0;
    j = 0;
    while (func[j]) { ps_sl_buf[i] = func[j]; i = i + 1; j = j + 1; }
    ps_sl_buf[i] = '.'; i = i + 1;
    j = 0;
    while (var[j]) { ps_sl_buf[i] = var[j]; i = i + 1; j = j + 1; }
    ps_sl_buf[i] = '.'; i = i + 1;
    /* append counter digits */
    v = ps_sl_count;
    if (v == 0) {
        ps_sl_buf[i] = '0'; i = i + 1;
    } else {
        d = 0;
        while (v > 0) { digits[d] = '0' + (v % 10); d = d + 1; v = v / 10; }
        while (d > 0) { d = d - 1; ps_sl_buf[i] = digits[d]; i = i + 1; }
    }
    ps_sl_buf[i] = 0;
    ps_sl_count = ps_sl_count + 1;
}

/* Size of the object an already-parsed sizeof operand denotes.
 * Arrays report their stored total size; a row of a pointer-to-array
 * member reports cols * element size; everything else is ty_size. */
static void sema_expr(Node *n);    /* sema.h: sizeof's operand gets its C type */
static int sema_vty(Node *x);
static long long sema_cvalof(Node *c);
static void sema_convert_diag(Node *n, char *cls, char *msg);
static char ps_implicit_msg[240];
static int ps_sizeof_node_mty(Node *n);

/* sizeof expr: the size of the operand's C type, which sema works out
 * (the operand is not evaluated, and is discarded) -- so sizeof(+c) is
 * int's, sizeof(d + d) double's, sizeof(1 << ll) int's, where the size
 * of the machine type had been the operand's before promotion (W12).
 * Over SQLite the two sizes agree everywhere.  (The one difference first
 * seen there was a C-type bug, not the old size's: a (**name) declarator
 * typed with one pointer, so aExt[0] looked like a function, size 1 --
 * caught reading the assembly diff, fixed in ps_fnptr_cty.)
 * S12CC_CTY_CHECK=1 still reports each difference from the old size. */
static int ps_sizeof_node(Node *n) {
    int old;
    int cs;
    if (n->kind == ND_MEMBER && n->bit_width > 0)
        p_error("invalid application of 'sizeof' to bit-field");   /* 6.5.3.4p1 */
    old = ps_sizeof_node_mty(n);
    sema_expr(n);
    if (n->cty <= 0) return old;
    cs = ct_sizeof(n->cty);
    if (cs != old) {
        if (ps_cty_check < 0) ps_cty_check = getenv("S12CC_CTY_CHECK") != 0;
        if (ps_cty_check) {
            fdputs("sizeof-mismatch kind=", 2);
            fdputuint(2, (unsigned)n->kind);
            fdputs(" ty=", 2);
            fdputuint(2, (unsigned)n->ty);
            fdputs(" old=", 2);
            fdputuint(2, (unsigned)old);
            fdputs(" c=", 2);
            fdputuint(2, (unsigned)cs);
            fdputs("\n", 2);
        }
    }
    /* An object -- a variable, member or element, an lvalue -- keeps
     * the size its declaration recorded; a value an operator computed
     * takes its C type's, which is where the promotions live.  (The C
     * types of declarations are still being completed: a global array
     * of function pointers was typed as a pointer, and an all-C-type
     * sizeof turned its length 3 into 1.) */
    /* ... except *p where p points to an array, which the declarations
     * never recorded: its C type is the array's (int (*q)[3]: 12) */
    if (n->kind == ND_UNARY && n->op == TK_STAR && ct_is_array(n->cty) && cs > 0) return cs;
    if ((n->attr & AT_LV) || ct_is_array(n->cty) || ct_is_func(n->cty)) return old;
    return cs;
}

static int ps_sizeof_node_mty(Node *n) {
    int li;
    int gi;
    if (n->kind == ND_VAR && n->is_array) {
        if (n->is_local) {
            li = find_local(n->name);
            return (li >= 0) ? ps_lsize[li] : ty_size(n->ty);
        }
        gi = find_global(n->name);
        return (gi >= 0) ? ps_gsize[gi] : ty_size(n->ty);
    }
    if (n->kind == ND_MEMBER && n->is_array) return n->val_hi;
    /* a string literal is an array of char, its bytes and the NUL (C90
     * 6.1.4): sizeof("abc") is 4 by coincidence, sizeof("A") 2, not the
     * pointer the literal decays to (selfhost ISSUES-84) */
    if (n->kind == ND_STRING) return lex_str_len[n->val] + 1;
    if (n->kind == ND_BINOP && n->arr_cols > 0 && ty_is_ptr(n->ty))
        return n->arr_cols * ty_size(ty_deref(n->ty));
    return ty_size(n->ty);
}

/* Lexer mark and rewind, for a parse that must look past the current
 * token before it commits: ps_lex_mark saves the position, then either
 * ps_lex_rewind returns to it or ps_lex_commit keeps what was read.
 * Marks nest, PS_MK_MAX deep: a constant tried as an expression may hold
 * a type whose dimension is another.  The six probes that saved these
 * fields by hand, all but the line number, now use it: a rewind across
 * a newline counted it twice, and __LINE__ ran a few ahead in SQLite. */
#define PS_MK_MAX 8
static int ps_mk_depth;
static int ps_mk_tok[PS_MK_MAX];
static int ps_mk_bol[PS_MK_MAX];
static int ps_mk_val[PS_MK_MAX];
static int ps_mk_slen[PS_MK_MAX];
static int ps_mk_rcs[PS_MK_MAX];
static int ps_mk_ract[PS_MK_MAX];
static int ps_mk_line[PS_MK_MAX];
static char *ps_mk_rp[PS_MK_MAX];
static char *ps_mk_rts[PS_MK_MAX];
static char *ps_mk_rte[PS_MK_MAX];
static char ps_mk_str[PS_MK_MAX * LEX_STR_SZ];
/* the current token's literal: its high words, suffixes and type, and
 * whether it was painted.  Not saved, a rewind onto 1.0 or 2LL kept the
 * last literal's high word (1.0/4 + 2.0 was 2.5 once a static
 * initializer was read twice). */
#define PS_MK_NUM 9
static int ps_mk_num[PS_MK_MAX * PS_MK_NUM];

static void ps_mk_num_save(int *v) {
    v[0] = lex_val_hi; v[1] = lex_val_ll; v[2] = lex_val_l; v[3] = lex_val_u;
    v[4] = lex_val_hexoct; v[5] = lex_val_macro; v[6] = lex_fval_hi; v[7] = lex_fty;
    v[8] = lex_painted;
}

static void ps_mk_num_restore(int *v) {
    lex_val_hi = v[0]; lex_val_ll = v[1]; lex_val_l = v[2]; lex_val_u = v[3];
    lex_val_hexoct = v[4]; lex_val_macro = v[5]; lex_fval_hi = v[6]; lex_fty = v[7];
    lex_painted = v[8];
}

static void ps_lex_mark(void) {
    int d;
    d = ps_mk_depth;
    if (d >= PS_MK_MAX) p_error("lexer marks nested too deep");
    ps_mk_tok[d] = lex_tok; ps_mk_val[d] = lex_val; ps_mk_slen[d] = lex_slen;
    ps_mk_rcs[d] = lex_rcs; ps_mk_ract[d] = lex_ract;
    ps_mk_rp[d] = lex_rp; ps_mk_rts[d] = lex_rts; ps_mk_rte[d] = lex_rte;
    ps_mk_line[d] = lex_line;
    ps_mk_bol[d] = lex_bol;
    memcpy(ps_mk_str + d * LEX_STR_SZ, lex_str, lex_slen + 1);
    ps_mk_num_save(ps_mk_num + d * PS_MK_NUM);
    pp_cond_save(d);
    ps_mk_depth = d + 1;
    pp_peek_depth = pp_peek_depth + 1;
}

static void ps_lex_commit(void) {
    ps_mk_depth = ps_mk_depth - 1;
    pp_peek_depth = pp_peek_depth - 1;
}

static void ps_lex_rewind(void) {
    int d;
    d = ps_mk_depth - 1;
    lex_tok = ps_mk_tok[d]; lex_val = ps_mk_val[d]; lex_slen = ps_mk_slen[d];
    lex_rcs = ps_mk_rcs[d]; lex_ract = ps_mk_ract[d];
    lex_rp = ps_mk_rp[d]; lex_rts = ps_mk_rts[d]; lex_rte = ps_mk_rte[d];
    lex_line = ps_mk_line[d];   /* the newlines read past the mark come again */
    lex_bol = ps_mk_bol[d];
    memcpy(lex_str, ps_mk_str + d * LEX_STR_SZ, ps_mk_slen[d] + 1);
    ps_mk_num_restore(ps_mk_num + d * PS_MK_NUM);
    pp_cond_restore(d);     /* directives read past the mark run again */
    ps_mk_depth = d;
    pp_peek_depth = pp_peek_depth - 1;
}

/* The constant evaluator is 32-bit.  A long long literal keeps its high
 * word here: pc_wide says the last leaf was such a literal and pc_hi is
 * its high word (negated along with the low word by unary minus);
 * pc_nleaf counts leaves, so a caller can tell one literal -- maybe
 * negated, maybe in parentheses -- from an expression, whose value is
 * 32-bit and gets sign-extended. */
static int pc_hi;
static int pc_wide;
static int pc_nleaf;
/* The pointee type of the most recent (T*) cast the constant
 * evaluator consumed, so &((T*)K)[i] can scale i.  -1 = none. */
static int pc_cast_ty;

/* After the offsetof keyword: ( type , member ) -> the member's byte
 * offset.  Shared by the expression parser and the constant evaluator
 * (SQLite sizes an array with offsetof(KeyInfo,aColl)). */
static int parse_offsetof_value(void) {
    int oty;
    int omi;
    char omn[256];
    expect(TK_LPAREN);
    oty = parse_type();
    if (!ty_is_struct(oty)) {
        p_error("offsetof requires struct/union type");
        return 0;
    }
    expect(TK_COMMA);
    if (lex_tok != TK_IDENT) {
        p_error("expected member name in offsetof");
        return 0;
    }
    memcpy(omn, lex_str, lex_slen + 1);
    next();
    omi = find_member(oty, omn);
    if (omi < 0) {
        p_error("unknown member in offsetof");
        return 0;
    }
    if (stm_bit_width[omi] > 0) {
        p_error("offsetof of bit-field is not allowed");
        return 0;
    }
    expect(TK_RPAREN);
    return stm_off[omi];
}

/* sizeof ( type-name ), cursor after the type parse_type read (ty, and
 * tdn its ps_type_arrcount): a typedef'd array type is its whole array
 * -- sizeof(jmp_buf) was 4, the size of the pointer such a type is
 * spelled as -- and an abstract array declarator multiplies:
 * sizeof(int[5]), sizeof(jmp_buf[4]). */
static int ps_abstract_fnptr(int ty);
static int ps_sizeof_type_tail(int ty, int tdn) {
    int v;
    if (ty_is_incomplete_struct(ty) && lex_tok == TK_RPAREN)
        p_error("invalid application of 'sizeof' to an incomplete type");   /* 6.5.3.4p1 */
    if (tdn > 0 && ty_is_ptr(ty) && lex_tok != TK_LPAREN) {
        v = ty_size(ty_deref(ty)) * tdn;
    } else {
        ty = ps_abstract_fnptr(ty);
        skip_decl_qualifiers();
        v = ty_size(ty);
        if (ps_type_cty > 0 && ct_is_array(ps_type_cty) && tdn == 0)
            v = ct_sizeof(ps_type_cty);      /* (*[2])(void): the array's */
    }
    while (lex_tok == TK_LBRACK) {
        next();
        v = v * parse_const_int();
        expect(TK_RBRACK);
    }
    return v;
}

/* The array dimensions [N][M]... after an abstract declarator, as the
 * C type of an array of elem: the first dimension is the outermost. */
static int ps_abstract_dims(int elem) {
    int n;
    if (lex_tok != TK_LBRACK) return elem;
    next();
    n = parse_const_int();
    expect(TK_RBRACK);
    return ct_array(ps_abstract_dims(elem), n);
}

/* After a type in a cast or sizeof: a parenthesized abstract declarator.
 * T (*)(params) or T (**)(params) -- SQLite's (void(*)(void*))0 -- is an
 * opaque pointer here; T (*)[N]... is a pointer to an array, one pointer
 * wide (the [N] was left for sizeof to multiply: sizeof(int (*)[3]) was
 * 12, W11).  Its machine type is a pointer to the element; the C type
 * says how far p[1] is from p, and ps_rowptr_value gives a value of it
 * the row width and strides a declared T (*p)[N]... has. */
static int ps_abs_rowptr;

/* n, a value whose C type the cast or va_arg just read: when that is a
 * pointer to an array, the elements one step moves (arr_cols) and the
 * strides below (arr_sub), so n[i] is a row as p[i] is on a declared
 * pointer to an array.  Refused until 2026-10-30. */
static void ps_rowptr_value(Node *n) {
    int x;
    if (!ps_abs_rowptr || n->cty <= 0 || !ct_is_ptr(n->cty)) return;
    x = ct_baseof(n->cty);
    if (!ct_is_array(x) || ps_cty_scalars(x) <= 0) return;
    n->arr_cols = ps_cty_scalars(x);
    n->arr_sub = ps_cty_sub(x);
}

static int ps_abstract_fnptr(int ty) {
    int nstar;
    int ndim;
    ps_abs_rowptr = 0;
    if (lex_tok != TK_LPAREN) return ty;
    next();
    if (lex_tok != TK_STAR) { p_error("expected * in abstract declarator"); return ty; }
    nstar = 0;
    while (lex_tok == TK_STAR) { next(); skip_decl_qualifiers(); nstar = nstar + 1; }
    /* (*[N])(params): an array of N function pointers (6.7.7) */
    ndim = -1;
    if (lex_tok == TK_LBRACK) {
        next();
        ndim = ps_array_size();
        expect(TK_RBRACK);
    }
    expect(TK_RPAREN);
    if (lex_tok == TK_LBRACK) {
        ps_type_cty = ps_abstract_dims(ps_type_cty);
        ty = ty + TY_PTR * nstar;
        ps_abs_rowptr = 1;
    } else {
        if (lex_tok == TK_LPAREN) { next(); p_skip_to_rparen(); }
        /* the C type: that many pointers to a function returning the
         * type read so far (its parameters are not recorded yet) */
        ps_type_cty = ct_func(ps_type_cty, 0, -1, 0);
        ty = TY_PTR + TY_INT;
    }
    while (nstar > 0) { ps_type_cty = ct_ptr(ps_type_cty); nstar = nstar - 1; }
    if (ndim >= 0) ps_type_cty = ct_array(ps_type_cty, ndim);
    return ty;
}

static int parse_const_primary(void) {
    int ci;
    int ty;
    int idx;
    int esz;

    if (lex_tok == TK_LPAREN) {
        /* Grouping parens are not a leaf: (5000000000LL) is still one
         * literal. Counting them as a leaf made parse_const_ll_hi
         * sign-extend the low word (GitHub issue 17).
         *
         * Postfix [] binds to the parenthesized expression, so
         * ((T*)K)[i] and (((T*)K)[i]) are the same address constant.
         * Without consuming [i] here, expect(')') hits the '['
         * (GitHub issue 62). */
        next();
        ci = parse_const_int();
        esz = 1;
        if (pc_cast_ty >= 0 && ty_is_ptr(pc_cast_ty)) {
            esz = ty_size(ty_deref(pc_cast_ty));
            if (esz <= 0) esz = 1;
        }
        while (lex_tok == TK_LBRACK) {
            next();
            idx = parse_const_int();
            expect(TK_RBRACK);
            ci = ci + idx * esz;
            pc_wide = 0;
            pc_hi = 0;
        }
        expect(TK_RPAREN);
        return ci;
    }
    pc_wide = 0;
    pc_nleaf = pc_nleaf + 1;
    if (lex_tok == TK_OFFSETOF) {
        next();
        return parse_offsetof_value();
    }
    if (lex_tok == TK_SIZEOF) {
        next();
        if (lex_tok != TK_LPAREN) {
            /* sizeof unary-expression, no parens (C99 6.5.3.4) */
            Node *sn;
            sn = parse_unary();
            return ps_sizeof_node(sn);
        }
        expect(TK_LPAREN);
        if (is_type()) {
            int sz_tdn;
            int sz_v;
            ty = parse_type();
            sz_tdn = ps_type_arrcount;
            while (lex_tok == TK_STAR) { ty = ty + TY_PTR; sz_tdn = 0; next(); skip_decl_qualifiers(); }
            sz_v = ps_sizeof_type_tail(ty, sz_tdn);
            expect(TK_RPAREN);
            return sz_v;
        }
        /* sizeof(expression) in a constant context -- global initializers
         * like `sizeof(tab) / sizeof(tab[0])` (rogue's mon_table_len).
         * Build the node just to read its type/size, then discard it. */
        {
            Node *sn;
            int sv;
            sn = parse_expr();
            expect(TK_RPAREN);
            sv = ps_sizeof_node(parse_postfix_from(sn));
            return sv;
        }
    }
    if (lex_tok == TK_FNUM) {
        double fa;
        int fop;
        int rv;
        int fw[2];
        fw[0] = lex_val;
        fw[1] = lex_fval_hi;
        memcpy(&fa, fw, 8);
        next();
        while (lex_tok == TK_STAR || lex_tok == TK_SLASH) {
            fop = lex_tok;
            next();
            rv = parse_const_unary();
            if (fop == TK_STAR) fa = fa * (double)rv;
            else fa = fa / (double)rv;
        }
        return (int)fa;
    }
    if (lex_tok == TK_NUM || lex_tok == TK_CHARLIT) {
        ci = lex_val;
        if (lex_tok == TK_NUM &&
            (lex_val_hi != 0 || lex_val_ll || lex_val_u ||
             (lex_val_hi == 0 && lex_val < 0 && !lex_val_macro))) {
            /* U/LL, a high word, or a 32-bit value with bit 31 set
             * (0x80000000 / 2147483648): keep high word 0 rather than
             * sign-extending into a long long (GitHub #17).  A #define
             * that stored -5 as an int is lex_val_macro and must still
             * sign-extend. */
            pc_hi = lex_val_hi;
            pc_wide = 1;
        }
        next();
        return ci;
    }
    if (lex_tok == TK_IDENT) {
        ci = find_const(lex_str);
        if (ci >= 0) {
            next();
            return ps_cval[ci];
        }
    }
    p_error("expected constant integer");
    return 0;
}

/* Compile-time FP constant folding for integer contexts: doom's
 * am_map casts scaled FP constants to fixed_t in a global table,
 * (fixed_t)(-.867*R).  A TK_FNUM primary folds an immediate * / /
 * chain in double arithmetic, then truncates.  (Native doubles:
 * host-built cross compilers use hardware FP; the stage07-built
 * bootstrap lowers to the __fp64 soft-float calls it already
 * links.) */
static int parse_const_unary(void) {
    int ty;

    if (lex_tok == TK_PLUS) {
        next();
        return parse_const_unary();
    }
    if (lex_tok == TK_TILDE) {
        /* ~0L (SQLite shell's ctrlMask); invert the high word too */
        int nv;
        next();
        nv = parse_const_unary();
        if (pc_wide) pc_hi = ~pc_hi;
        return ~nv;
    }
    if (lex_tok == TK_BANG) {
        int nv;
        next();
        nv = parse_const_unary();
        if (pc_wide && pc_hi != 0) nv = 1;
        pc_wide = 0;
        pc_hi = 0;
        return nv == 0;
    }
    if (lex_tok == TK_MINUS) {
        int nv;
        next();
        nv = parse_const_unary();
        if (pc_wide) {
            /* negate the 64-bit pair: -(hi:lo) */
            pc_hi = ~pc_hi;
            if (nv == 0) pc_hi = pc_hi + 1;
        }
        return 0 - nv;
    }
    if (lex_tok == TK_AMP) {
        /* &((T*)K)[i] -- an integer constant wearing a pointer, folded to
         * K + i * sizeof(T).  SQLite spells SQLITE_INT_TO_PTR(X) as
         * ((void*)&((char*)0)[X]) on a compiler that defines neither
         * __PTRDIFF_TYPE__ nor __GNUC__, which is us, and every
         * aBuiltinFunc entry is one.  (Until GitHub issue 39 was fixed
         * the trailing comment on that #elif made us take the #else
         * instead, so this shape never reached the evaluator.)
         *
         * & over a real symbol is a relocation and does not come here:
         * an initializer tries parse_global_init_symbol_reloc_at first,
         * including &(sym) (GitHub issue 59). Extra grouping around
         * this fold, &(((T*)K)[i]), is postfix [] inside grouping
         * parens (GitHub issue 62). */
        int base;
        int idx;
        int esz;
        next();
        pc_cast_ty = -1;
        base = parse_const_unary();
        esz = 1;
        if (pc_cast_ty >= 0 && ty_is_ptr(pc_cast_ty)) {
            esz = ty_size(ty_deref(pc_cast_ty));
            if (esz <= 0) esz = 1;
        }
        pc_wide = 0;
        pc_hi = 0;
        if (lex_tok == TK_LBRACK) {
            next();
            idx = parse_const_int();
            expect(TK_RBRACK);
            pc_wide = 0;
            pc_hi = 0;
            return base + idx * esz;
        }
        return base;
    }
    if (lex_tok == TK_LPAREN) {
        ps_lex_mark();
        next();
        if (is_type()) {
            ty = parse_type();
            ty = ps_declarator_stars(ty, ps_type_cty);
            ty = ps_abstract_fnptr(ty);
            skip_decl_qualifiers();
            expect(TK_RPAREN);
            ps_lex_commit();
            pc_cast_ty = ty;
            if (ct_kindof(ct_unqual(ps_type_cty)) == CT_BOOL) {
                /* (_Bool)v is 0 or 1 (6.3.1.2); W6 replaces this
                 * evaluator with sema's typed one (docs/TYPES.md S3) */
                ty = parse_const_unary();
                ty = (ty != 0 || pc_hi_of(ty) != 0);
                pc_wide = 0;
                pc_hi = 0;
                return ty;
            }
            return parse_const_unary();
        }
        ps_lex_rewind();
    }
    return parse_const_primary();
}

/* the high word of the constant just parsed: its own when it is wide,
 * else the low word's sign extended */
static int pc_hi_of(int lo) {
    if (pc_wide) return pc_hi;
    if (lo < 0) return -1;
    return 0;
}

/* * / % stay 32-bit: a wide operand is taken only when its value fits
 * 32 bits, and one that does not is an error, not a silently dropped
 * high word (which is what + and - did to INT64_MIN's usual spelling,
 * -9223372036854775807LL - 1, until they learned 64 bits below). */
static void pc_narrow_check(int lo) {
    if (pc_hi_of(lo) != (lo < 0 ? -1 : 0))
        p_error("64-bit constant multiply or divide is not supported");
    pc_wide = 0;
    pc_hi = 0;
}

static int parse_const_mul(void) {
    int v;
    int r;

    v = parse_const_unary();
    while (lex_tok == TK_STAR || lex_tok == TK_SLASH || lex_tok == TK_PERCENT) {
        pc_narrow_check(v);
        if (lex_tok == TK_STAR) {
            next();
            r = parse_const_unary();
            pc_narrow_check(r);
            v = v * r;
        } else if (lex_tok == TK_SLASH) {
            next();
            r = parse_const_unary();
            pc_narrow_check(r);
            if (r == 0) p_error("division by zero in constant expression");
            v = v / r;
        } else {
            next();
            r = parse_const_unary();
            pc_narrow_check(r);
            if (r == 0) p_error("division by zero in constant expression");
            v = v % r;
        }
    }
    return v;
}

/* Constant-expression precedence chain: mul < add < shift < and <
 * xor < or.  Doom's headers lean on shifts ((1<<FRACBITS), (7<<29))
 * and masks in constant initializers. */
static int parse_const_add(void) {
    int v;
    int r;
    int op;
    int lwide;
    int lhi;
    int rhi;
    unsigned ulo;
    unsigned uhi;
    unsigned rlo;
    unsigned sum;

    v = parse_const_mul();
    while (lex_tok == TK_PLUS || lex_tok == TK_MINUS) {
        /* 64 bits when either side is wide: INT64_MIN is spelled
         * -9223372036854775807LL - 1, and the 32-bit sum made it 0 */
        op = lex_tok;
        lwide = pc_wide;
        lhi = pc_hi_of(v);
        next();
        r = parse_const_mul();
        if (!lwide && !pc_wide) {
            if (op == TK_PLUS) v = v + r;
            else v = v - r;
        } else {
            rhi = pc_hi_of(r);
            ulo = v;
            uhi = lhi;
            rlo = r;
            if (op == TK_PLUS) {
                sum = ulo + rlo;
                uhi = uhi + rhi;
                if (sum < ulo) uhi = uhi + 1;
            } else {
                sum = ulo - rlo;
                uhi = uhi - rhi;
                if (ulo < rlo) uhi = uhi - 1;
            }
            v = sum;
            pc_hi = uhi;
            pc_wide = 1;
        }
    }
    return v;
}

static int parse_const_shift(void) {
    int v;
    int s;
    int op;
    int lo;
    int hi;
    int wide;
    int sh;
    unsigned ulo;
    unsigned uhi;

    v = parse_const_add();
    while (lex_tok == TK_LSHIFT || lex_tok == TK_RSHIFT) {
        op = lex_tok;
        /* The right-hand addend is another leaf and would clear pc_wide
         * before we can use the left operand's high word.  Save first.
         * A 32-bit `1 << 40` drops the bit; 1LL << 32 must land in pc_hi
         * (GitHub issue 49). */
        lo = v;
        wide = pc_wide;
        hi = wide ? pc_hi : 0;
        next();
        s = parse_const_add();
        if (!wide && s >= 0 && s < 32) {
            if (op == TK_LSHIFT) v = lo << s;
            else v = lo >> s;
        } else {
            sh = s;
            if (sh < 0) sh = 0;
            ulo = lo;
            uhi = hi;
            if (op == TK_LSHIFT) {
                if (sh >= 64) {
                    ulo = 0;
                    uhi = 0;
                } else if (sh >= 32) {
                    uhi = ulo << (sh - 32);
                    ulo = 0;
                } else if (sh != 0) {
                    uhi = (uhi << sh) | (ulo >> (32 - sh));
                    ulo = ulo << sh;
                }
            } else if (sh >= 64) {
                if (wide && hi < 0) {
                    ulo = -1;
                    uhi = -1;
                } else {
                    ulo = 0;
                    uhi = 0;
                }
            } else if (sh >= 32) {
                ulo = ((int)uhi) >> (sh - 32);
                if (wide && hi < 0) uhi = -1;
                else uhi = 0;
            } else if (sh != 0) {
                ulo = (ulo >> sh) | (uhi << (32 - sh));
                uhi = ((int)uhi) >> sh;
            }
            v = ulo;
            pc_hi = uhi;
            pc_wide = 1;
        }
    }
    return v;
}

/* Relational and equality sit between shift and `&` in C's precedence
   ladder.  They were missing entirely: the chain jumped from shift
   straight to `&`, so any file-scope initializer using a comparison or a
   ternary failed with "expected constant integer" -- while the very same
   expression inside a function compiled, because that path uses the full
   expression parser.  libutf's xterm-256 palette is built by
   XC(v) = ((v) == 0 ? 0 : 55 + 40 * (v)) in a static table, which is how
   this surfaced. */
static int parse_const_rel(void) {
    int v;

    v = parse_const_shift();
    while (lex_tok == TK_LT || lex_tok == TK_GT ||
           lex_tok == TK_LE || lex_tok == TK_GE) {
        if (lex_tok == TK_LT) {
            next();
            v = (v < parse_const_shift());
        } else if (lex_tok == TK_GT) {
            next();
            v = (v > parse_const_shift());
        } else if (lex_tok == TK_LE) {
            next();
            v = (v <= parse_const_shift());
        } else {
            next();
            v = (v >= parse_const_shift());
        }
        pc_wide = 0;   /* 0 or 1: no high word from an operand */
        pc_hi = 0;
    }
    return v;
}

static int parse_const_eq(void) {
    int v;

    v = parse_const_rel();
    while (lex_tok == TK_EQ || lex_tok == TK_NE) {
        if (lex_tok == TK_EQ) {
            next();
            v = (v == parse_const_rel());
        } else {
            next();
            v = (v != parse_const_rel());
        }
        pc_wide = 0;   /* 0 or 1: no high word from an operand */
        pc_hi = 0;
    }
    return v;
}

static int parse_const_band(void) {
    int v;
    int r;
    int lwide;
    int lhi;

    v = parse_const_eq();
    while (lex_tok == TK_AMP) {
        lwide = pc_wide;
        lhi = pc_hi_of(v);
        next();
        r = parse_const_eq();
        if (lwide || pc_wide) {                 /* both words */
            pc_hi = lhi & pc_hi_of(r);
            pc_wide = 1;
        }
        v = v & r;
    }
    return v;
}

static int parse_const_bxor(void) {
    int v;
    int r;
    int lwide;
    int lhi;

    v = parse_const_band();
    while (lex_tok == TK_CARET) {
        lwide = pc_wide;
        lhi = pc_hi_of(v);
        next();
        r = parse_const_band();
        if (lwide || pc_wide) {                 /* both words */
            pc_hi = lhi ^ pc_hi_of(r);
            pc_wide = 1;
        }
        v = v ^ r;
    }
    return v;
}

static int parse_const_bor(void) {
    int v;
    int r;
    int lwide;
    int lhi;

    v = parse_const_bxor();
    while (lex_tok == TK_PIPE) {
        lwide = pc_wide;
        lhi = pc_hi_of(v);
        next();
        r = parse_const_bxor();
        if (lwide || pc_wide) {                 /* both words */
            pc_hi = lhi | pc_hi_of(r);
            pc_wide = 1;
        }
        v = v | r;
    }
    return v;
}

static int parse_const_land(void) {
    int v;

    v = parse_const_bor();
    while (lex_tok == TK_LAND) {
        next();
        v = (parse_const_bor() != 0 && v != 0);
        pc_wide = 0;
        pc_hi = 0;
    }
    return v;
}

static int parse_const_lor(void) {
    int v;

    v = parse_const_land();
    while (lex_tok == TK_LOR) {
        next();
        v = (parse_const_land() != 0 || v != 0);
        pc_wide = 0;
        pc_hi = 0;
    }
    return v;
}

/* Both arms are parsed -- their tokens have to be consumed either way --
   and the selected one is returned.  So this does not short-circuit: a
   `1 ? 0 : (1/0)` still reports division by zero.  Constant expressions
   have no side effects, so that is the only visible difference, and it
   only rejects code that did not compile at all before. */
static int parse_const_cond(void) {
    int c;
    int a;
    int b;
    int awide;
    int ahi;

    c = parse_const_lor();
    if (lex_tok == TK_QMARK) {
        next();
        a = parse_const_cond();
        awide = pc_wide;
        ahi = pc_hi;
        expect(TK_COLON);
        b = parse_const_cond();
        if (c != 0) {
            pc_wide = awide;
            pc_hi = ahi;
            return a;
        }
        return b;
    }
    return c;
}

/* A constant expression evaluated as C evaluates it (W6): parsed as an
 * expression, typed by sema, its value sema's (cval, in the expression's
 * type -- 0x7FFFFFFF * 2LL is a long long, and the token evaluator
 * below, 32-bit, made it -2).  Returns 0, the lexer back where it was,
 * when sema does not find an integer constant expression: the token
 * evaluator then takes the address constants and casts sema does not
 * fold (&((T *)0)->m, (char *)&x + 4). */
static Node *parse_conditional(void);

/* Is the cursor on a lone literal, maybe negated, that ends the
 * constant -- 5, or -1.5, then , } ; ] : or )?  Its token: TK_NUM, or
 * TK_FNUM for a double literal; 0 otherwise.  The token evaluators read
 * those as C does, and the large tables of them in SQLite would each
 * build an expression tree (it ran out of memory).  A float literal is
 * not one: its token holds float bits, which the FP token path took
 * for a double's. */
static int ps_const_lone_literal(void) {
    int tok;
    int t;
    if (lex_tok != TK_NUM && lex_tok != TK_FNUM && lex_tok != TK_MINUS) return 0;
    ps_lex_mark();
    if (lex_tok == TK_MINUS) next();
    tok = lex_tok;
    t = 0;
    if (tok == TK_NUM || (tok == TK_FNUM && !ty_is_float(lex_fty))) {
        next();
        t = lex_tok == TK_COMMA || lex_tok == TK_RBRACE || lex_tok == TK_SEMI ||
            lex_tok == TK_RBRACK || lex_tok == TK_COLON || lex_tok == TK_RPAREN;
    }
    ps_lex_rewind();
    return t ? tok : 0;
}

static int ps_const_typed(int *lo, int *hi) {
    Node *n;
    if (ps_const_lone_literal()) return 0;
    ps_lex_mark();
    n = parse_conditional();
    sema_expr(n);
    if ((n->attr & AT_ICX) == 0) {
        ps_lex_rewind();
        return 0;
    }
    ps_lex_commit();
    *lo = n->cval;
    *hi = n->cval_hi;
    return 1;
}

/* _Static_assert's expression: an integer constant expression (6.7.10p3),
 * evaluated -- it was a literal or taken as true. */
static int ps_static_assert_value(Node *e) {
    sema_expr(e);
    if (!(e->attr & AT_ICX)) p_error("static assertion expression is not an integral constant expression");
    if (e->cval == 0 && e->cval_hi == 0) p_error("static assertion failed");
    return 1;
}

/* _Static_assert among a structure's members (6.7.2.1p1). */
static void ps_member_static_assert(void) {
    next();
    expect(TK_LPAREN);
    ps_static_assert_value(parse_assign());
    expect(TK_COMMA);
    while (lex_tok == TK_STRING) next();
    expect(TK_RPAREN);
    expect(TK_SEMI);
}

/* A constant that must be an integer constant expression -- a case
 * label (6.8.4.2p3), an enumerator's value (6.7.2.2p2): typed by sema
 * first, then evaluated as before.  case 1.5 was 1, and E = 2.5 was 2. */
static int parse_const_ll_hi(int *lo);
static void ps_require_integer_const(char *what) {
    Node *n;
    ps_lex_mark();
    n = parse_conditional();
    sema_expr(n);
    ps_lex_rewind();
    if (!ct_is_integer(sema_vty(n))) p_error(what);
}

static int ps_case_value(int *lo) {
    ps_require_integer_const("case label is not an integer constant expression");
    return parse_const_ll_hi(lo);
}

static int ps_enum_value(void) {
    ps_require_integer_const("enumerator value is not an integer constant expression");
    return parse_const_int();
}

/* Parse a compile-time constant integer (for initializers and array
 * sizes).  pc_wide/pc_hi carry the high word to parse_const_ll_hi. */
static int parse_const_int(void) {
    int lo;
    int hi;
    if (ps_const_typed(&lo, &hi)) {
        pc_wide = 1;
        pc_hi = hi;
        pc_nleaf = pc_nleaf + 1;
        return lo;
    }
    return parse_const_cond();
}

/* a constant for an 8-byte integer initializer: one long long literal's
 * own high word, otherwise the 32-bit constant expression sign-extended */
static int parse_const_ll_hi(int *lo) {
    int v;
    v = parse_const_int();
    *lo = v;
    /* A wide literal, unary on one, or a shift of one (1LL << 32)
     * leaves pc_wide set.  The old nleaf == n0+1 test dropped pc_hi
     * after a shift, because the count is a second leaf. */
    if (pc_wide) return pc_hi;
    if (v < 0) return -1;
    return 0;
}

static int ps_ginit_cur_off(int gidx) {
    if (ps_ginit_start[gidx] < 0) return 0;
    return ps_ginit_pool_len - ps_ginit_start[gidx];
}

static void ps_ginit_emit_byte(int v) {
    if (ps_ginit_pool_len >= PS_MAX_INIT_POOL) p_error("init pool overflow");
    ps_ginit_pool[ps_ginit_pool_len] = v & 255;
    ps_ginit_pool_len = ps_ginit_pool_len + 1;
}

static void ps_ginit_ensure_len(int gidx, int len) {
    while (ps_ginit_cur_off(gidx) < len) ps_ginit_emit_byte(0);
}

static void ps_ginit_store_byte_at(int gidx, int rel_off, int v) {
    int abs_off;
    if (rel_off < 0) p_error("negative initializer offset");
    ps_ginit_ensure_len(gidx, rel_off + 1);
    abs_off = ps_ginit_start[gidx] + rel_off;
    ps_ginit_pool[abs_off] = v & 255;
}

static void ps_ginit_store_int_at(int gidx, int rel_off, int v, int sz) {
    int i;
    i = 0;
    while (i < sz) {
        ps_ginit_store_byte_at(gidx, rel_off + i, (v >> (i * 8)) & 255);
        i = i + 1;
    }
}

/* A bit-field's value into a static initializer: its storage unit's
 * bytes read, the field's bits replaced, written back (little-endian,
 * as bf_alloc numbers the bits). */
static void ps_ginit_store_bits_at(int gidx, int rel_off, int mi, int v) {
    unsigned cur;
    unsigned mask;
    int usz;
    int i;
    int p;
    usz = ty_size(stm_type[mi]);
    ps_ginit_ensure_len(gidx, rel_off + usz);
    p = ps_ginit_start[gidx] + rel_off;
    cur = 0;
    i = usz - 1;
    while (i >= 0) {
        cur = (cur << 8) | (unsigned)(ps_ginit_pool[p + i] & 255);
        i = i - 1;
    }
    mask = stm_bit_width[mi] >= 32 ? 4294967295u : (1u << stm_bit_width[mi]) - 1u;
    cur = (cur & ~(mask << stm_bit_off[mi])) | (((unsigned)v & mask) << stm_bit_off[mi]);
    i = 0;
    while (i < usz) {
        ps_ginit_pool[p + i] = cur & 255;
        cur = cur >> 8;
        i = i + 1;
    }
}

/* The member a designator ended on, when it is a bit-field: the
 * initializer loops store it as one.  -1 for none. */
static int ps_desig_bf = -1;

/* A bit-field member mi of a static initializer, at rel_off (its unit):
 * an integer constant expression, in braces or not. */
/* A static bit-field's initializer, read once to be graded at the
 * field's width (sema_const_conv) and put back for the reader. */
static void ps_bf_init_check(int mi) {
    Node *n;
    ps_lex_mark();
    n = parse_assign();
    sema_expr(n);
    sema_const_conv(n, stm_cty[mi], n, stm_bit_width[mi]);
    ps_lex_rewind();
}

static void ps_ginit_bitfield(int gidx, int rel_off, int mi) {
    int v;
    int br;
    br = lex_tok == TK_LBRACE;
    if (br) next();
    ps_bf_init_check(mi);
    v = parse_const_int();
    if (stm_cty[mi] > 0 && ct_kindof(stm_cty[mi]) == CT_BOOL) v = v != 0;   /* 6.3.1.2: _Bool b : 1 = 2 is 1 */
    if (br) {
        if (lex_tok == TK_COMMA) next();
        expect(TK_RBRACE);
    }
    ps_ginit_store_bits_at(gidx, rel_off, mi, v);
}

static void ps_ginit_begin(int gidx) {
    ps_init_cty = ps_gcty[gidx];        /* the walk's first reader takes it */
    ps_mark_global_init(gidx);
    ps_ginit_start[gidx] = ps_ginit_pool_len;
    ps_ginit_count[gidx] = 0;
    ps_girel_start[gidx] = ps_ngirelocs;
    ps_girel_count[gidx] = 0;
}

static void ps_ginit_finish(int gidx) {
    ps_ginit_count[gidx] = ps_ginit_pool_len - ps_ginit_start[gidx];
    ps_girel_count[gidx] = ps_ngirelocs - ps_girel_start[gidx];
}

/* Track where the most recent reloc landed so an addend can be
 * attached after insertion (insertion is position-sorted). */
static int ps_girel_last_pos;
static void ps_ginit_insert_reloc_at(int gidx, int rel_off, int kind,
                                     int idx, char *name, int sz) {
    int pos;
    int i;

    if (rel_off < 0) p_error("negative initializer offset");
    if (ps_ngirelocs >= PS_MAX_INIT_RELOCS) p_error("too many init relocs");
    ps_ginit_ensure_len(gidx, rel_off + sz);
    pos = ps_girel_start[gidx];
    while (pos < ps_ngirelocs && ps_girel_off[pos] <= rel_off) pos = pos + 1;
    i = ps_ngirelocs;
    while (i > pos) {
        ps_girel_off[i] = ps_girel_off[i - 1];
        ps_girel_kind[i] = ps_girel_kind[i - 1];
        ps_girel_idx[i] = ps_girel_idx[i - 1];
        ps_girel_add[i] = ps_girel_add[i - 1];
        ps_girel_size[i] = ps_girel_size[i - 1];
        ps_girel_name[i] = ps_girel_name[i - 1];
        i = i - 1;
    }
    ps_girel_off[pos] = rel_off;
    ps_girel_add[pos] = 0;
    ps_girel_last_pos = pos;
    ps_girel_kind[pos] = kind;
    ps_girel_idx[pos] = idx;
    ps_girel_size[pos] = sz;
    if (name) ps_girel_name[pos] = strdup(name);
    else ps_girel_name[pos] = NULL;
    ps_ngirelocs = ps_ngirelocs + 1;
}

static void ps_ginit_add_reloc_at(int gidx, int rel_off, int kind, int idx, int sz) {
    ps_ginit_insert_reloc_at(gidx, rel_off, kind, idx, NULL, sz);
}

/* Compile-time f64->f32 bit conversion (integer-only; truncating
 * mantissa round, subnormals flush to zero).  For float globals with
 * double literals: `float mouse_acceleration = 2.0;` */
static int ps_f64_to_f32_bits(int lo, int hi) {
    int sign;
    int exp;
    unsigned int mant;

    sign = (hi >> 31) & 1;
    exp = (hi >> 20) & 2047;
    if (exp == 0) return sign << 31;                  /* zero/subnormal */
    if (exp == 2047) {
        if ((hi & 1048575) != 0 || lo != 0)
            return (sign << 31) | 0x7FC00000;         /* nan */
        return (sign << 31) | 0x7F800000;             /* inf */
    }
    exp = exp - 1023 + 127;
    if (exp <= 0) return sign << 31;                  /* underflow */
    if (exp >= 255) return (sign << 31) | 0x7F800000; /* overflow */
    mant = (((unsigned int)hi & 1048575u) << 3) | (((unsigned int)lo >> 29) & 7u);
    return (sign << 31) | (exp << 23) | (int)mant;
}

static void ps_ginit_add_sym_reloc_at(int gidx, int rel_off, char *name, int sz) {
    ps_ginit_insert_reloc_at(gidx, rel_off, GIRELOC_SYMBOL, 0, name, sz);
}



static int try_consume_type_cast(void) {
    int ty;

    if (lex_tok != TK_LPAREN) return 0;
    ps_lex_mark();

    next();
    if (!is_type()) {
        ps_lex_rewind();
        return 0;
    }
    ty = parse_type();
    (void)ty;
    while (lex_tok == TK_STAR) { next(); skip_decl_qualifiers(); }
    (void)ps_abstract_fnptr(0);
    skip_decl_qualifiers();
    expect(TK_RPAREN);
    ps_lex_commit();
    return 1;
}

/* --- Address constants (6.6p9) --- */

/* An address constant's base, from ps_addr_value/ps_addr_lvalue: a
 * named symbol, or a string literal (pool index), and a byte offset. */
static char *ps_ac_sym;
static int   ps_ac_str;      /* the string's pool index; -1 = not a string */
static int   ps_ac_off;

static int ps_addr_value(Node *n);

/* The parser's row of a 2-D array, m[i]: m plus i times the row width,
 * counted in elements, as an address constant (&m[1][0], &m[1]).  It
 * fell to the older readers, which took `m` for a redeclaration. */
static int ps_addr_row(Node *n) {
    int t;
    int esz;
    if (!n->lhs || !n->rhs || !(n->rhs->attr & AT_ICX) || !ps_addr_value(n->lhs)) return 0;
    t = ct_decay(sema_vty(n->lhs));
    if (!ct_is_ptr(t)) return 0;
    t = ct_baseof(t);
    while (ct_is_array(t)) t = ct_baseof(t);
    esz = ct_sizeof(t);
    if (esz <= 0) return 0;
    ps_ac_off = ps_ac_off + (int)sema_cvalof(n->rhs) * esz;
    return 1;
}

/* The address of lvalue n: a global or static local, a member of one
 * (its offset added), or what a pointer constant points at. */
static int ps_addr_lvalue(Node *n) {
    char *ss;
    if (n->kind == ND_VAR) {
        if (n->is_local) {
            ss = ps_static_local_sym(n->name);
            if (!ss) return 0;      /* an automatic object has no constant address */
            ps_ac_sym = ss;
        } else {
            ps_ac_sym = n->name;
        }
        ps_ac_str = -1;
        ps_ac_off = 0;
        return 1;
    }
    if (n->kind == ND_STRING) {
        ps_ac_sym = NULL;
        ps_ac_str = n->val;
        ps_ac_off = 0;
        return 1;
    }
    if (n->kind == ND_MEMBER && n->lhs && n->bit_width == 0) {
        if (!ps_addr_lvalue(n->lhs)) return 0;
        ps_ac_off = ps_ac_off + n->val;
        return 1;
    }
    if (n->kind == ND_UNARY && n->op == TK_STAR && n->lhs) return ps_addr_value(n->lhs);
    if (n->kind == ND_BINOP && n->op == TK_PLUS && n->arr_cols > 0) return ps_addr_row(n);
    return 0;
}

/* The value of n as an address constant: an array or function
 * designator, &lvalue, a cast of one, or one plus or minus an integer
 * constant expression scaled by what it points to. */
static int ps_addr_value(Node *n) {
    int t;
    int esz;
    long long c;
    if (n->kind == ND_CAST && n->lhs) return ps_addr_value(n->lhs);
    if (n->kind == ND_FUNC_REF) {
        ps_ac_sym = n->name;
        ps_ac_str = -1;
        ps_ac_off = 0;
        return 1;
    }
    if (n->kind == ND_STRING) return ps_addr_lvalue(n);
    /* an array decays to its address, a function designator likewise */
    if (n->cty > 0 && (ct_is_array(n->cty) || (n->kind == ND_VAR && ct_is_func(n->cty))))
        return ps_addr_lvalue(n);
    if (n->kind == ND_COMMA && n->op == TK_LBRACE && n->rhs) return ps_addr_value(n->rhs);
    if (n->kind == ND_UNARY && n->op == TK_AMP && n->lhs) return ps_addr_lvalue(n->lhs);
    if (n->kind == ND_BINOP && (n->op == TK_PLUS || n->op == TK_MINUS) && n->lhs && n->rhs) {
        if ((n->rhs->attr & AT_ICX) && ct_is_ptr(sema_vty(n->lhs))) {
            if (!ps_addr_value(n->lhs)) return 0;
            t = ct_baseof(sema_vty(n->lhs));
            c = sema_cvalof(n->rhs);
        } else if (n->op == TK_PLUS && (n->lhs->attr & AT_ICX) && ct_is_ptr(sema_vty(n->rhs))) {
            if (!ps_addr_value(n->rhs)) return 0;
            t = ct_baseof(sema_vty(n->rhs));
            c = sema_cvalof(n->lhs);
        } else {
            return 0;
        }
        esz = ct_kindof(t) == CT_VOID ? 1 : ct_sizeof(t);
        if (esz <= 0) return 0;
        if (n->op == TK_MINUS) c = 0 - c;
        ps_ac_off = ps_ac_off + (int)c * esz;
        return 1;
    }
    return 0;
}

/* A static initializer that is an address constant (6.6p9, 6.7.9p4):
 * the whole expression, typed by sema, a symbol or string plus a byte
 * offset -- garr + 4, &gs.b[2], &garr[5] - 2, "hello" + 1,
 * (char *)&gs + sizeof(int).  The token-level reader below took a name
 * and stopped at the `+` or the `.`.  Rewinds and returns 0 for
 * anything else, which the older paths then read as before. */
static int ps_init_addr_const(int gidx, int rel_off, int sz) {
    Node *n;
    ps_lex_mark();
    n = parse_assign();
    sema_expr(n);
    if (!ps_addr_value(n) || (lex_tok != TK_COMMA && lex_tok != TK_RBRACE && lex_tok != TK_SEMI)) {
        ps_lex_rewind();
        return 0;
    }
    ps_lex_commit();
    if (ps_ac_str >= 0) {
        ps_ginit_add_reloc_at(gidx, rel_off, GIRELOC_STRING, ps_ac_str, sz);
    } else {
        ps_ginit_add_sym_reloc_at(gidx, rel_off, ps_ac_sym, sz);
    }
    ps_girel_add[ps_girel_last_pos] = ps_ac_off;
    return 1;
}

/* Is the string literal just read followed by + or - (after any
 * literals it concatenates with)?  Then the initializer is "hello" + 1,
 * an address constant, not the string.  Read from the raw text: lexing
 * a string adds it to the pool. */
static int ps_string_then_offset(void) {
    char *p;
    p = lex_rp;
    while (1) {
        while (*p == 32 || *p == 9 || *p == 10 || *p == 13) p = p + 1;
        if (*p != 34) break;
        p = p + 1;
        while (*p != 0 && *p != 34) {
            if (*p == 92 && p[1] != 0) p = p + 1;
            p = p + 1;
        }
        if (*p == 34) p = p + 1;
    }
    return *p == 43 || *p == 45;
}

/* A file-scope scalar's initializer that is not a literal or a name:
 * for a pointer, an address constant if it is one ((char *)&gs + 4),
 * otherwise an integer constant as before. */
static void ps_scalar_global_init(int idx, int ty) {
    if (ps_gcty[idx] > 0 && ct_kindof(ps_gcty[idx]) == CT_BOOL) {
        ps_ginit_begin(idx);
        ps_init_bool_at(idx, 0);
        ps_ginit_ensure_len(idx, 1);
        ps_ginit_finish(idx);
        return;
    }
    if (ty_is_ptr(ty) && lex_tok != TK_NUM) {
        ps_ginit_begin(idx);
        if (ps_init_addr_const(idx, 0, ty_size(ty))) {
            ps_ginit_ensure_len(idx, ty_size(ty));
            ps_ginit_finish(idx);
            return;
        }
        ps_ginit_start[idx] = -1;       /* not one: as if never begun */
        ps_ghasinit[idx] = 0;
    }
    ps_set_scalar_init(idx, parse_const_int(), 0);
}

static int parse_global_init_symbol_reloc_at(int gidx, int rel_off, int sz) {
    char nm[256];
    int ci;
    int amp;
    int gi2;
    int esz;
    int sidx;
    int nparen;
    int got_idx;

    if (ps_init_addr_const(gidx, rel_off, sz)) return 1;
    while (try_consume_type_cast()) {
    }
    amp = 0;
    nparen = 0;
    if (lex_tok == TK_AMP) {
        /* Speculative: only a & over a NAME is a relocation.  The other
         * shape is an integer constant wearing a pointer --
         * &((char*)0)[i], SQLite's SQLITE_INT_TO_PTR -- which used to
         * hard-error here.  Rewind and let the caller constant-fold it
         * (parse_const_unary folds the same form).  pp_peek_depth keeps
         * a macro expansion in place while the rewind point is live.
         *
         * Grouping parens around the name are the same address constant:
         * &(A) and &((A)) are &A (GitHub issue 59).  Extra parens around
         * a cast-of-zero (&(((char*)0)[i]), GitHub issue 62) do not
         * leave an identifier here, so we rewind and let the fold path
         * take them. */
        ps_lex_mark();
        amp = 1;
        next();
        while (try_consume_type_cast()) {
        }
        while (lex_tok == TK_LPAREN) {
            next();
            nparen = nparen + 1;
        }
        if (lex_tok != TK_IDENT ||
            (find_const(lex_str) >= 0 && nparen > 0)) {
            /* not a name, or &(SOME_ENUM): not a relocation; the '&'
             * comes back so the constant-fold path sees the parens */
            ps_lex_rewind();
            return 0;
        }
        ps_lex_commit();
    }
    if (lex_tok != TK_IDENT) {
        return 0;
    }
    ci = find_const(lex_str);
    if (ci >= 0) return 0;
    memcpy(nm, lex_str, lex_slen + 1);
    next();
    if (ps_static_local_sym(nm)) strcpy(nm, ps_static_local_sym(nm));
    ps_ginit_add_sym_reloc_at(gidx, rel_off, nm, sz);
    /* &sym[const]: address constant with a byte addend
     * (tables.c: const fixed_t *finecosine = &finesine[FINEANGLES/4]).
     * The subscript may sit inside grouping (&(sym[i])) or after it
     * (&sym[i], &(sym)[i] -- postfix [] binds tighter than unary &). */
    got_idx = 0;
    if (amp && lex_tok == TK_LBRACK) {
        next();
        sidx = parse_const_int();
        expect(TK_RBRACK);
        esz = 4;
        gi2 = find_global(nm);
        if (gi2 >= 0 && ty_is_ptr(ps_gtype[gi2]))
            esz = ty_size(ty_deref(ps_gtype[gi2]));
        ps_girel_add[ps_girel_last_pos] = sidx * esz;
        got_idx = 1;
    }
    while (nparen > 0) {
        expect(TK_RPAREN);
        nparen = nparen - 1;
    }
    if (amp && !got_idx && lex_tok == TK_LBRACK) {
        next();
        sidx = parse_const_int();
        expect(TK_RBRACK);
        esz = 4;
        gi2 = find_global(nm);
        if (gi2 >= 0 && ty_is_ptr(ps_gtype[gi2]))
            esz = ty_size(ty_deref(ps_gtype[gi2]));
        ps_girel_add[ps_girel_last_pos] = sidx * esz;
    }
    return 1;
}

/* A pointer object's initializer: a relocation when it names a symbol,
 * otherwise an address constant folded to an integer -- SQLite's
 * SQLITE_INT_TO_PTR(X), ((void*)&((char*)0)[X]).  Erroring instead was
 * fine only while GitHub issue 39 kept that #elif branch unselected. */
static void ps_ptr_init_at(int gidx, int ty) {
    ps_ginit_begin(gidx);
    if (!parse_global_init_symbol_reloc_at(gidx, 0, ty_size(ty)))
        ps_ginit_store_int_at(gidx, 0, parse_const_int(), ty_size(ty));
    ps_ginit_ensure_len(gidx, ty_size(ty));
    ps_ginit_finish(gidx);
}

static void parse_global_init_value(int ty, int gidx);
static void parse_global_init_value_at(int ty, int arr_count, int gidx, int rel_off);
static int parse_global_init_array_at(int elem_ty, int count, int gidx, int base_rel);
static int parse_global_init_array2d_at(int elem_ty, int rows, int cols, int sub, int gidx, int base_rel);
static void parse_global_init_struct_at(int ty, int gidx, int base_rel);

/* Initializers checked as assignments are (6.7.9p11): each scalar an
 * initializer list sets, converted to its target's C type and graded
 * as sema grades an assignment.  The readers here walk machine types,
 * so the target's C type rides beside them in ps_init_cty -- set by
 * whoever knows it (ps_ginit_begin, local_init_begin, the walk itself)
 * and taken by the reader it is meant for.  An array's is the innermost
 * element's (the walks hold arrays flat); 0 is unknown, not checked. */
static void sema_assign_check(Node *at, int lt, Node *r);    /* sema.h */
static int sema_fp_const(Node *n, double *out);             /* sema.h */
static int ps_addr_value(Node *n);

static int ps_init_take(void) {
    int c;
    c = ps_init_cty;
    ps_init_cty = 0;
    return c;
}

static int ps_init_elem_cty(int c) {
    if (c <= 0) return 0;
    while (ct_is_array(c)) c = ct_baseof(c);
    return c;
}

/* Is a compound literal next, (T){...}?  Not read twice: it defines
 * an object. */
static int ps_compound_literal_ahead(void) {
    int r;
    if (lex_tok != TK_LPAREN) return 0;
    ps_lex_mark();
    next();
    r = 0;
    if (is_type()) {
        parse_type();
        while (lex_tok == TK_STAR) { next(); skip_decl_qualifiers(); }
        (void)ps_abstract_fnptr(0);
        skip_decl_qualifiers();
        if (lex_tok == TK_RPAREN) {
            next();
            r = lex_tok == TK_LBRACE;
        }
    }
    ps_lex_rewind();
    return r;
}

/* A static _Bool's initializer, converted as C converts to _Bool
 * (6.3.1.2): 0 or 1 -- an integer or floating constant compared with
 * zero, an address constant never null.  The integer reader stored the
 * value itself, and _Bool b = 2 held 2. */
static void ps_init_bool_at(int gidx, int rel_off) {
    Node *n;
    double d;
    int v;
    n = parse_assign();
    sema_expr(n);
    if (n->attr & AT_ICX) v = sema_cvalof(n) != 0;
    else if (sema_fp_const(n, &d)) v = d != 0.0;
    else if (ps_addr_value(n)) v = 1;
    else {
        p_error("initializer element is not a compile-time constant");
        v = 0;
    }
    ps_ginit_store_int_at(gidx, rel_off, v, 1);
}

/* A static object's scalar initializer, checked against its target's
 * C type c: read as an expression, graded, and the lexer put back for
 * the readers that store it.  Only where the C type agrees with the
 * machine type ty the reader has -- where the two walks part, nothing
 * is said. */
static void ps_init_check(int c, int ty) {
    Node *n;
    if (c <= 0 || !ct_is_scalar(c) || !ct_mty_agrees(c, ty)) return;
    if (lex_tok == TK_LBRACE || ps_compound_literal_ahead()) return;
    ps_lex_mark();
    n = parse_assign();
    sema_expr(n);
    sema_assign_check(n, ct_unqual(c), n);
    ps_lex_rewind();
}

/* A string longer than the character array it initializes, its
 * terminating null aside (6.7.9p14 allows dropping only that): clang
 * warns, and so does this. */
static void ps_init_string_fits(int sp_idx, int count) {
    if (lex_str_width[sp_idx] == 1 && count >= 0 && lex_str_len[sp_idx] > count)
        sema_convert_diag(nd_num(0), "str-too-long", "initializer-string for char array is too long");
}

static void parse_global_init_designator(int base_ty, int base_arr_count,
                                         int *rel_out, int *ty_out,
                                         int *arr_count_out, int *root_out) {
    int ty;
    int arr_count;
    int rel;
    int idx;
    int elem_ty;
    int mi;
    int si;
    char dnm[256];

    ty = base_ty;
    arr_count = base_arr_count;
    rel = 0;
    *root_out = -1;
    ps_desig_cty = ps_init_take();      /* the aggregate's */
    while (lex_tok == TK_LBRACK || lex_tok == TK_DOT) {
        if (lex_tok == TK_LBRACK) {
            next();
            idx = parse_const_int();
            if (idx < 0) p_error("negative array designator");
            expect(TK_RBRACK);
            if (!ty_is_ptr(ty) || arr_count == 0)
                p_error("array designator on non-array");
            if (ps_cty_rows(ps_desig_cty)) {
                /* a row of a multi-dimensional array, held flat: the
                 * step is a whole row, and the row is what remains */
                if (ct_countof(ps_desig_cty) > 0 && idx >= ct_countof(ps_desig_cty))
                    p_error("array designator out of range");
                ps_desig_cty = ct_baseof(ps_desig_cty);
                rel = rel + idx * ct_sizeof(ps_desig_cty);
                arr_count = ps_cty_scalars(ps_desig_cty);
                if (*root_out < 0) *root_out = idx;
                continue;
            }
            if (arr_count > 0 && idx >= arr_count) p_error("array designator out of range");
            elem_ty = ty_deref(ty);
            rel = rel + (idx * ty_size(elem_ty));
            ty = elem_ty;
            arr_count = 0;
            ps_desig_cty = ps_init_elem_cty(ps_desig_cty);
            if (*root_out < 0) *root_out = idx;
        } else {
            next();
            if (lex_tok != TK_IDENT) p_error("expected field name in initializer");
            memcpy(dnm, lex_str, lex_slen + 1);
            next();
            if (!ty_is_struct(ty)) p_error("field designator on non-struct");
            si = ty_struct_idx(ty);
            mi = find_member(ty, dnm);
            if (mi < 0) p_error("unknown field in initializer");
            if (struct_member_is_flexible_array(mi))
                p_error("flexible array initializer unsupported");
            ps_desig_bf = stm_bit_width[mi] > 0 ? mi : -1;
            rel = rel + stm_off[mi];
            ty = stm_type[mi];
            arr_count = struct_member_array_count(mi);
            ps_desig_cty = stm_cty[mi];
            if (*root_out < 0) *root_out = struct_member_nth_for_idx(si, mi);
        }
    }
    *rel_out = rel;
    *ty_out = ty;
    *arr_count_out = arr_count;
    ps_init_cty = ps_desig_cty;         /* the designated object's */
}

/* A brace-elided initializer ends before a designator: `, [i] =` or
 * `, .m =` names a subobject of the enclosing braced list, not of the
 * elided one (6.7.9p17, p20).  Is one next, past a comma? */
static int ps_comma_then_designator(void) {
    int r;
    if (lex_tok != TK_COMMA) return 0;
    ps_lex_mark();
    next();
    r = lex_tok == TK_LBRACK || lex_tok == TK_DOT;
    ps_lex_rewind();
    return r;
}

static int parse_global_init_array_at(int elem_ty, int count, int gidx, int base_rel) {
    int ec;             /* the element's C type */
    int i;
    int max_i;
    int elem_sz;
    int sp_idx;
    int slen;
    char *sp;
    int rel;
    int target_ty;
    int target_arr_count;
    int root;
    int has_brace;

    elem_sz = ty_size(elem_ty);
    ec = ps_init_elem_cty(ps_init_take());
    if ((elem_ty & TY_BASE_MASK) == TY_CHAR && lex_tok == TK_STRING) {
        sp_idx = parse_string_literal();
        slen = lex_str_len[sp_idx];
        ps_init_string_fits(sp_idx, count);
        if (count < 0) count = slen + 1;
        sp = lex_strpool + lex_str_off[sp_idx];
        i = 0;
        while (i < count) {
            if (i < slen) ps_ginit_store_byte_at(gidx, base_rel + i, sp[i] & 255);
            else ps_ginit_store_byte_at(gidx, base_rel + i, 0);
            i = i + 1;
        }
        return count;
    }
    /* C99 6.7.8 ¶22 brace elision: a flat-form parent initializer may
     * omit the braces around an array sub-aggregate, in which case we
     * consume tokens from the parent's brace list until we are full or
     * the parent's `}` appears.  has_brace=0 is that mode. */
    has_brace = (lex_tok == TK_LBRACE);
    if (has_brace) next();
    i = 0;
    max_i = 0;
    while (lex_tok != TK_EOF) {
        if (lex_tok == TK_RBRACE) break;
        if (!has_brace && count >= 0 && i >= count) break;
        if (i > 0) {
            if (has_brace) {
                expect(TK_COMMA);
                if (lex_tok == TK_RBRACE) break;
            } else {
                if (lex_tok != TK_COMMA) break;
                if (ps_comma_then_designator()) break;
                expect(TK_COMMA);
            }
        }
        if (lex_tok == TK_LBRACK) {
            ps_init_cty = ec > 0 ? ct_array(ec, -1) : 0;
            parse_global_init_designator(elem_ty + TY_PTR, count, &rel,
                                         &target_ty, &target_arr_count, &root);
            expect(TK_ASSIGN);
            parse_global_init_value_at(target_ty, target_arr_count, gidx, base_rel + rel);
            if (root >= 0) {
                i = root + 1;
                if (i > max_i) max_i = i;
            }
        } else {
            if (has_brace && count >= 0 && i >= count) p_error("too many initializers");
            ps_init_cty = ec;
            parse_global_init_value_at(elem_ty, 0, gidx, base_rel + (i * elem_sz));
            i = i + 1;
            if (i > max_i) max_i = i;
        }
    }
    if (has_brace) {
        if (lex_tok == TK_COMMA) next();
        expect(TK_RBRACE);
    }
    if (count < 0) count = max_i;
    ps_ginit_ensure_len(gidx, base_rel + (count * elem_sz));
    return count;
}

/* 2D array initializer: { {row}, {row}, ... }.  Each row is delegated
 * to parse_global_init_array_at, which already handles both braced
 * rows and C99 brace-elided flat rows.  Returns the row count (rows
 * may come in as -1 for an inferred first dimension, as in
 * vecscope's `static const int ship[][2] = {...}`). */
/* One subobject of a multi-dimensional global's initializer: n elements
 * elem_ty shaped by sub.  In braces and itself an array of rows, read
 * as one; otherwise n elements, braced or flat (6.7.9p20). */
static void ps_ginit_shaped(int elem_ty, int n, int sub, int gidx, int rel) {
    if (sub > 0 && lex_tok == TK_LBRACE)
        parse_global_init_array2d_at(elem_ty, n / ast_sub_cols[sub], ast_sub_cols[sub],
                                     ast_sub_next[sub], gidx, rel);
    else
        parse_global_init_array_at(elem_ty, n, gidx, rel);
}

/* A braced initializer for rows of cols elements elem_ty (rows -1:
 * as many as it has), each row shaped by sub past two dimensions.  The
 * position is kept in elements: a designator ([i], [i][j] ... by the
 * array's C type) moves it past the subobject it names, and what
 * follows continues there (6.7.9p17), mid-row if it lands there --
 * a braced sub-row at a sub-row's start, else elements flat to the
 * row's end.  Returns the rows. */
static int parse_global_init_array2d_at(int elem_ty, int rows, int cols, int sub, int gidx, int base_rel) {
    int pos;
    int max_pos;
    int first;
    int esz;
    int ec;
    int rel;
    int tty;
    int tac;
    int root;
    int rc;             /* a row's C type, for designators */
    int in_row;

    esz = ty_size(elem_ty);
    ec = ps_init_elem_cty(ps_init_take());
    rc = ps_rows_cty(ec > 0 ? ec : ps_obj_cty(elem_ty), cols, sub);
    expect(TK_LBRACE);
    pos = 0;
    max_pos = 0;
    first = 1;
    while (lex_tok != TK_EOF) {
        if (lex_tok == TK_RBRACE) break;
        if (!first) {
            expect(TK_COMMA);
            if (lex_tok == TK_RBRACE) break;
        }
        first = 0;
        in_row = pos % cols;
        if (lex_tok == TK_LBRACK) {
            ps_init_cty = ct_array(rc, rows);
            parse_global_init_designator(elem_ty + TY_PTR, rows >= 0 ? rows * cols : -1,
                                         &rel, &tty, &tac, &root);
            expect(TK_ASSIGN);
            parse_global_init_value_at(tty, tac, gidx, base_rel + rel);
            pos = (rel + (tac > 0 ? tac : 1) * esz) / esz;
        } else if (in_row != 0 && sub > 0 && lex_tok == TK_LBRACE &&
                   in_row % ast_sub_cols[sub] == 0) {
            ps_init_cty = ec;
            ps_ginit_shaped(elem_ty, ast_sub_cols[sub], ast_sub_next[sub], gidx, base_rel + pos * esz);
            pos = pos + ast_sub_cols[sub];
        } else if (in_row != 0) {
            ps_init_cty = ec;
            parse_global_init_array_at(elem_ty, cols - in_row, gidx, base_rel + pos * esz);
            pos = pos + cols - in_row;
        } else {
            if (rows >= 0 && pos / cols >= rows) p_error("too many 2D initializer rows");
            ps_init_cty = ec;
            ps_ginit_shaped(elem_ty, cols, sub, gidx, base_rel + pos * esz);
            pos = pos + cols;
        }
        if (pos > max_pos) max_pos = pos;
    }
    if (lex_tok == TK_COMMA) next();
    expect(TK_RBRACE);
    if (rows < 0) rows = (max_pos + cols - 1) / cols;
    ps_ginit_ensure_len(gidx, base_rel + rows * cols * esz);
    return rows;
}

static void parse_global_init_struct_at(int ty, int gidx, int base_rel) {
    int sc;             /* the structure's C type: a designator starts there */
    int si;
    int nf;
    int i;
    int mi;
    int field_ty;
    int arr_count;
    int has_brace;
    int rel;
    int target_ty;
    int target_arr_count;
    int root;

    si = ty_struct_idx(ty);
    nf = st_nfields[si];
    sc = ps_init_take();
    /* Brace is optional for nested struct fields inside a flat-form parent
     * initializer (e.g. `struct Rect gr = {10, 20, 30, 40}`). */
    has_brace = (lex_tok == TK_LBRACE);
    if (has_brace) next();
    i = 0;
    while (lex_tok != TK_EOF) {
        if (has_brace && lex_tok == TK_RBRACE) break;
        if (!has_brace && i >= nf) break;
        if (i > 0) {
            if (has_brace) {
                expect(TK_COMMA);
                if (lex_tok == TK_RBRACE) break;
            } else {
                if (lex_tok != TK_COMMA) break;
                expect(TK_COMMA);
            }
        }
        if (lex_tok == TK_DOT) {
            ps_desig_bf = -1;
            ps_init_cty = sc;
            parse_global_init_designator(ty, 0, &rel, &target_ty,
                                         &target_arr_count, &root);
            expect(TK_ASSIGN);
            if (ps_desig_bf >= 0) ps_ginit_bitfield(gidx, base_rel + rel, ps_desig_bf);
            else parse_global_init_value_at(target_ty, target_arr_count, gidx, base_rel + rel);
            if (root >= 0) i = root + 1;
            else i = i + 1;
        } else {
            if (i >= nf) p_error("too many initializers");
            mi = struct_field_nth_idx(si, i);
            if (mi < 0) p_error("missing struct field");
            if (struct_member_is_flexible_array(mi))
                p_error("flexible array initializer unsupported");
            if (stm_bit_width[mi] > 0) {
                ps_ginit_bitfield(gidx, base_rel + stm_off[mi], mi);
                i = i + 1;
                continue;
            }
            field_ty = stm_type[mi];
            arr_count = struct_member_array_count(mi);
            ps_init_cty = stm_cty[mi];
            if (stm_arr_cols[mi] > 0 && lex_tok == TK_LBRACE) {
                /* 2D member array: rows of cols elements */
                parse_global_init_array2d_at(ty_deref(field_ty),
                    arr_count / stm_arr_cols[mi], stm_arr_cols[mi], stm_arr_sub[mi],
                    gidx, base_rel + stm_off[mi]);
            } else {
                parse_global_init_value_at(field_ty, arr_count, gidx, base_rel + stm_off[mi]);
            }
            i = i + 1;
        }
        if (st_is_union[si]) break;
    }
    if (has_brace) {
        if (lex_tok == TK_COMMA) next();
        expect(TK_RBRACE);
    }
    ps_ginit_ensure_len(gidx, base_rel + ty_size(ty));
}

static void parse_global_init_struct(int ty, int gidx) {
    parse_global_init_struct_at(ty, gidx, ps_ginit_cur_off(gidx));
}

/* FP initializer element: [+|-] (FP literal | integer const-expr),
 * stored as IEEE bytes at rel_off — 8 for double, 4 for float (the
 * float narrowing is a native (float) cast, so it round-to-nearest
 * matches clang, unlike the truncating ps_f64_to_f32_bits).  Integer
 * constants convert through a native double ((double)2 for
 * `double x = 2;`), which the old int-truncating fallthrough silently
 * corrupted. */
/* __builtin_nan[f|l](...), __builtin_inf[f|l](), __builtin_huge_val[f|l]()
 * at the cursor, in a static initializer: consumed, *lo:*hi the double's
 * bits (a float item narrows them by cast, NaN and infinity surviving).
 * The expression parser folds the same names (selfhost ISSUES-71). */
static int ps_fp_builtin_bits(int *lo, int *hi) {
    int nan;
    if (lex_tok != TK_IDENT) return 0;
    nan = strcmp(lex_str, "__builtin_nanf") == 0 || strcmp(lex_str, "__builtin_nan") == 0 || strcmp(lex_str, "__builtin_nanl") == 0;
    if (!nan && strcmp(lex_str, "__builtin_inff") != 0 && strcmp(lex_str, "__builtin_inf") != 0 &&
        strcmp(lex_str, "__builtin_infl") != 0 && strcmp(lex_str, "__builtin_huge_valf") != 0 &&
        strcmp(lex_str, "__builtin_huge_val") != 0 && strcmp(lex_str, "__builtin_huge_vall") != 0) return 0;
    next();
    expect(TK_LPAREN);
    while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next();   /* a NaN's payload string: not read */
    expect(TK_RPAREN);
    *lo = 0;
    *hi = nan ? 0x7FF80000 : 0x7FF00000;
    return 1;
}

/* A floating static initializer evaluated as C evaluates it: parsed as
 * an expression, typed by sema, folded by sema_fp_const (W6).  Returns
 * 0, the lexer back where it was, for a lone literal (the token path
 * below reads it as C does) and for what sema_fp_const does not take. */
static int ps_fp_const_typed(double *d) {
    Node *n;
    /* a lone double literal only: an integer one, converted by the
     * token path, went through int (18446744073709551615ULL was -1) */
    if (ps_const_lone_literal() == TK_FNUM) return 0;
    ps_lex_mark();
    n = parse_conditional();
    sema_expr(n);
    if (!sema_fp_const(n, d)) {
        ps_lex_rewind();
        return 0;
    }
    ps_lex_commit();
    return 1;
}

static void ps_fp_init_store_at(int ty, int gidx, int rel_off) {
    int fneg;
    int flo;
    int fhi;
    int fw[2];
    int i;
    int fop;
    double fd;
    double rv;

    if (ps_fp_const_typed(&fd)) {
        memcpy(fw, &fd, 8);
        flo = fw[0];
        fhi = fw[1];
        goto store;
    }
    fneg = 0;
    if (lex_tok == TK_MINUS) { fneg = 1; next(); }
    else if (lex_tok == TK_PLUS) next();
    if (lex_tok == TK_FNUM) {
        flo = lex_val;
        fhi = lex_fval_hi;
        next();
    } else if (ps_fp_builtin_bits(&flo, &fhi)) {
        /* NAN, INFINITY */
    } else {
        fd = (double)parse_const_int();
        memcpy(fw, &fd, 8);
        flo = fw[0];
        fhi = fw[1];
    }
    if (fneg) fhi = fhi ^ (1 << 31);
    /* Fold an immediate * / / chain in double arithmetic (dtoa's
     * tinytens entry is `9007199254740992.*9007199254740992.e-256`).
     * The sign parsed above binds to the FIRST literal, as in C. */
    while (lex_tok == TK_STAR || lex_tok == TK_SLASH) {
        fop = lex_tok;
        next();
        fneg = 0;
        if (lex_tok == TK_MINUS) { fneg = 1; next(); }
        else if (lex_tok == TK_PLUS) next();
        if (lex_tok == TK_FNUM) {
            fw[0] = lex_val;
            fw[1] = fneg ? (lex_fval_hi ^ (1 << 31)) : lex_fval_hi;
            memcpy(&rv, fw, 8);
            next();
        } else {
            rv = (double)parse_const_int();
            if (fneg) rv = 0.0 - rv;
        }
        fw[0] = flo;
        fw[1] = fhi;
        memcpy(&fd, fw, 8);
        if (fop == TK_STAR) fd = fd * rv;
        else fd = fd / rv;
        memcpy(fw, &fd, 8);
        flo = fw[0];
        fhi = fw[1];
    }
store:
    if (ty_is_double(ty)) {
        i = 0;
        while (i < 4) {
            ps_ginit_store_byte_at(gidx, rel_off + i, (flo >> (i * 8)) & 255);
            ps_ginit_store_byte_at(gidx, rel_off + 4 + i, (fhi >> (i * 8)) & 255);
            i = i + 1;
        }
    } else {
        float ff;
        fw[0] = flo;
        fw[1] = fhi;
        memcpy(&fd, fw, 8);
        ff = (float)fd;
        memcpy(&flo, &ff, 4);
        i = 0;
        while (i < 4) {
            ps_ginit_store_byte_at(gidx, rel_off + i, (flo >> (i * 8)) & 255);
            i = i + 1;
        }
    }
}

/* Is ty a character type, the element of an array a string may initialize? */
static int ps_is_char_ty(int ty) {
    return (ty & TY_BASE_MASK) == TY_CHAR && (ty & TY_PTR_MASK) == 0;
}

/* A character array's initializer may be its string literal in braces,
 * { "hi" } (6.7.9p14); it was taken for a list of one element, and
 * char a[] = { "hi" } was one byte.  Before a string-literal test, for
 * a one-dimensional char array (in char m[2][8] = { "ab", "cd" } the
 * braces are the outer array's), ps_braced_string_open consumes a `{`
 * that a string follows and anything else is left as it was;
 * ps_braced_string_close, after the string, consumes the matching `}`.
 * The flag between them is a global so the callers -- parse_stmt and
 * parse_top_decl among them -- grow no locals. */
static int ps_str_braced;

static void ps_braced_string_open(int char_array) {
    ps_str_braced = 0;
    if (!char_array || lex_tok != TK_LBRACE) return;
    ps_lex_mark();
    next();
    if (lex_tok == TK_STRING) { ps_lex_commit(); ps_str_braced = 1; return; }
    ps_lex_rewind();
}

static void ps_braced_string_close(void) {
    if (!ps_str_braced) return;
    ps_str_braced = 0;
    if (lex_tok == TK_COMMA) next();
    expect(TK_RBRACE);
}

/* A cast in an initializer position: `(const unsigned char *)"abc"`.
   The cast cannot change the value -- the target type comes from the
   declaration being initialized -- so consume it and let the dispatch
   below see the real token.  Without this a cast string literal fell past
   every pointer case to the integer folder and died as "expected constant
   integer", which is what libutf's generated tables hit:
       const co_string_desc tr_nfd_ott[2045] =
           { { 3, 2, (const unsigned char *)"\x41\xCC\x80" }, ... };
   while the same initializer without the cast worked.  Same save-and-probe
   as parse_const_unary, since `(` may instead open a parenthesized
   constant expression. */
static int skip_init_cast(void) {

    if (lex_tok != TK_LPAREN) return 0;
    ps_lex_mark();
    next();
    if (is_type()) {
        parse_type();
        while (lex_tok == TK_STAR) { next(); skip_decl_qualifiers(); }
        (void)ps_abstract_fnptr(0);
        skip_decl_qualifiers();
        expect(TK_RPAREN);
        ps_lex_commit();
        return 1;
    }
    ps_lex_rewind();
    return 0;
}

/* Casts and grouping parens wrapping a *string* initializer.  Returns the
   number of grouping parens still needing a close, or -1 with the lexer
   restored if what follows is not a wrapped string literal.  libutf's
   generated NFD table spells its entries T(x) = ((const unsigned char *)(x)),
   so both wrapper kinds nest.  Probing rather than blanket paren-stripping
   matters: `(1 + 2) * 3` is also a parenthesized initializer, and eating its
   parens would strand the `* 3`. */
static int init_string_wrappers(void) {
    int depth;

    ps_lex_mark();

    depth = 0;
    while (lex_tok == TK_LPAREN) {
        next();
        if (is_type()) {
            parse_type();
            while (lex_tok == TK_STAR) { next(); skip_decl_qualifiers(); }
            (void)ps_abstract_fnptr(0);
            skip_decl_qualifiers();
            if (lex_tok != TK_RPAREN) break;
            next();                 /* a cast closes itself */
        } else {
            depth = depth + 1;      /* grouping paren, still open */
        }
    }
    if (lex_tok == TK_STRING) { ps_lex_commit(); return depth; }

    ps_lex_rewind();
    return -1;
}

static void parse_global_init_value_at(int ty, int arr_count, int gidx, int rel_off) {
    int v;
    int sp_idx;
    int wrap;
    int c;

    c = ps_init_take();
    if (!(arr_count != 0 && ty_is_ptr(ty)) && !ty_is_struct(ty)) ps_init_check(c, ty);
    if (c > 0 && ct_kindof(c) == CT_BOOL && arr_count == 0) {
        ps_init_bool_at(gidx, rel_off);
        return;
    }
    if (ty_is_ptr(ty) && lex_tok == TK_LPAREN) {
        wrap = init_string_wrappers();
        if (wrap >= 0) {
            sp_idx = parse_string_literal();
            while (wrap > 0) { expect(TK_RPAREN); wrap = wrap - 1; }
            ps_ginit_add_reloc_at(gidx, rel_off, GIRELOC_STRING, sp_idx,
                                  ty_size(ty));
            return;
        }
    }
    skip_init_cast();

    if (ps_cty_rows(c) && ty_is_ptr(ty) && lex_tok == TK_LBRACE) {
        /* an array of arrays in braces (a designated row of a bigger
         * one, a member's): its rows */
        ps_init_cty = c;
        parse_global_init_array2d_at(ty_deref(ty), ct_countof(c), ps_cty_scalars(ct_baseof(c)),
                                     ps_cty_sub(ct_baseof(c)), gidx, rel_off);
        return;
    }
    if (arr_count != 0 && ty_is_ptr(ty)) {
        ps_init_cty = c;
        parse_global_init_array_at(ty_deref(ty), arr_count, gidx, rel_off);
        return;
    }
    if (ty_is_struct(ty)) {
        ps_init_cty = c;
        parse_global_init_struct_at(ty, gidx, rel_off);
        return;
    }
    if (ty_is_ptr(ty) && lex_tok == TK_STRING) {
        sp_idx = parse_string_literal();
        ps_ginit_add_reloc_at(gidx, rel_off, GIRELOC_STRING, sp_idx, ty_size(ty));
        return;
    }
    if (ty_is_ptr(ty) && parse_global_init_symbol_reloc_at(gidx, rel_off, ty_size(ty))) {
        return;
    }
    /* Word-sized field initialized with a function or global name:
     * doom's info.c state table stores action functions in a union
     * whose member is a function-pointer typedef (flattened to int
     * here), e.g. {SPR_SHTG,4,0,{A_Light0},...}. */
    if (lex_tok == TK_IDENT && ty_size(ty) == 4 && find_const(lex_str) < 0 &&
        (is_known_func(lex_str) || find_global(lex_str) >= 0)) {
        parse_global_init_symbol_reloc_at(gidx, rel_off, 4);
        return;
    }
    if (ty_is_fp(ty)) {
        ps_fp_init_store_at(ty, gidx, rel_off);
        return;
    }
    if (ty_size(ty) == 8) {
        /* a long long element: two words, not one word's bytes repeated
         * (a 32-bit shift by 32 wraps here -- GitHub #11) */
        int hi;
        hi = parse_const_ll_hi(&v);
        ps_ginit_store_int_at(gidx, rel_off, v, 4);
        ps_ginit_store_int_at(gidx, rel_off + 4, hi, 4);
        return;
    }
    v = parse_const_int();
    ps_ginit_store_int_at(gidx, rel_off, v, ty_size(ty));
}

static void parse_global_init_value(int ty, int gidx) {
    parse_global_init_value_at(ty, 0, gidx, ps_ginit_cur_off(gidx));
}

/* Adjacent literals of which one is wide (6.4.5p5): one wide string,
 * a narrow piece's bytes widened to units.  Returns the pool index, or
 * -1 when all are narrow (the caller joins those as bytes). */
static int ps_wide_concat(int *idxs, int nidx) {
    int w;
    int i;
    int j;
    int units;
    int start;
    int wi;
    int src;
    w = 1;
    i = 0;
    while (i < nidx) {
        wi = lex_str_width[idxs[i]];
        /* no two different prefixes (6.4.5p2: u8 with a wide one; L
         * with U or u implementation-defined, and refused, as clang does) */
        j = 0;
        while (j < i) {
            if (lex_str_pfx[idxs[i]] && lex_str_pfx[idxs[j]] && lex_str_pfx[idxs[i]] != lex_str_pfx[idxs[j]])
                p_error("unsupported concatenation of string literals with different prefixes");
            j = j + 1;
        }
        if (wi > w) w = wi;
        i = i + 1;
    }
    if (w == 1) return -1;
    if (lex_str_count >= LEX_POOL_MAX) p_error("too many string literals");
    start = lex_strpool_len;
    units = 0;
    i = 0;
    while (i < nidx) {
        wi = lex_str_width[idxs[i]];
        src = lex_str_off[idxs[i]];
        if (wi == w) {
            j = 0;
            while (j < (lex_str_len[idxs[i]] + 1) - w) {      /* its units, not its terminator */
                lex_strpool[lex_strpool_len] = lex_strpool[src + j];
                lex_strpool_len = lex_strpool_len + 1;
                j = j + 1;
            }
            units = units + (lex_str_len[idxs[i]] + 1) / w - 1;
        } else {
            j = 0;
            while (j < lex_str_len[idxs[i]]) {
                lex_pool_unit(lex_strpool[src + j] & 255, w);
                j = j + 1;
            }
            units = units + lex_str_len[idxs[i]];
        }
        i = i + 1;
    }
    lex_pool_unit(0, w - 1);
    lex_strpool[lex_strpool_len] = 0;
    lex_strpool_len = lex_strpool_len + 1;
    lex_str_off[lex_str_count] = start;
    lex_str_len[lex_str_count] = units * w + w - 1;
    lex_str_width[lex_str_count] = w;
    lex_str_count = lex_str_count + 1;
    return lex_str_count - 1;
}

static int parse_string_literal(void) {
    static int idxs[1024];   /* was a 64-entry local: SQLite's shell builds one SQL statement from hundreds of adjacent literals; static because 4KB per frame of the recursive expression parser overran the stack */
    int nidx;
    int total;
    int start;
    int i;
    int j;
    int src;

    nidx = 0;
    total = 0;
    while (lex_tok == TK_STRING) {
        if (nidx >= 1024) p_error("too many adjacent string literals");
        idxs[nidx] = lex_val;
        total = total + lex_str_len[lex_val];
        nidx = nidx + 1;
        next();
    }
    if (nidx == 1) return idxs[0];
    i = ps_wide_concat(idxs, nidx);
    if (i >= 0) return i;
    if (lex_str_count >= LEX_POOL_MAX) p_error("too many string literals");
    if (lex_strpool_len + total + 1 > LEX_POOL_SZ) p_error("string pool overflow");
    start = lex_strpool_len;
    i = 0;
    while (i < nidx) {
        src = lex_str_off[idxs[i]];
        j = 0;
        while (j < lex_str_len[idxs[i]]) {
            lex_strpool[lex_strpool_len] = lex_strpool[src + j];
            lex_strpool_len = lex_strpool_len + 1;
            j = j + 1;
        }
        i = i + 1;
    }
    lex_strpool[lex_strpool_len] = 0;
    lex_strpool_len = lex_strpool_len + 1;
    i = lex_str_intern(start, total);
    if (i >= 0) {
        lex_strpool_len = start;    /* identical literal already pooled */
        return i;
    }
    lex_str_off[lex_str_count] = start;
    lex_str_len[lex_str_count] = total;
    lex_str_width[lex_str_count] = 1;
    lex_str_count = lex_str_count + 1;
    return lex_str_count - 1;
}

static int ps_str_contains(char *s, char *needle) {
    int i;
    int j;

    i = 0;
    if (needle[0] == 0) return 1;
    while (s[i]) {
        j = 0;
        while (s[i + j] && needle[j] && s[i + j] == needle[j]) {
            j = j + 1;
        }
        if (needle[j] == 0) return 1;
        i = i + 1;
    }
    return 0;
}

static int classify_gnu_asm(char *tmpl) {
    if (ps_str_contains(tmpl, "mrs") &&
        ps_str_contains(tmpl, "cntvct_el0")) {
        return ASM_A64_MRS_CNTVCT;
    }
    if (ps_str_contains(tmpl, "stp x29, x30") &&
        ps_str_contains(tmpl, "mov x20, x0") &&
        ps_str_contains(tmpl, "mov x21, x1") &&
        ps_str_contains(tmpl, "mov x22, x3") &&
        ps_str_contains(tmpl, "blr x2")) {
        return ASM_A64_DBT_TRAMPOLINE;
    }
    if (ps_str_contains(tmpl, "dc cvau")) {
        return ASM_A64_DC_CVAU;
    }
    if (ps_str_contains(tmpl, "ic ivau")) {
        return ASM_A64_IC_IVAU;
    }
    if (ps_str_contains(tmpl, "dsb ish")) {
        return ASM_A64_DSB_ISH;
    }
    if (ps_str_contains(tmpl, "isb")) {
        return ASM_A64_ISB;
    }
    /* x86-64 patterns. */
    if (ps_str_contains(tmpl, "rdtsc")) {
        return ASM_X64_RDTSC;
    }
    if (ps_str_contains(tmpl, "push %%rbp") &&
        ps_str_contains(tmpl, "mov %%rax, %%rbp") &&
        ps_str_contains(tmpl, "mov %%rsi, %%r13") &&
        ps_str_contains(tmpl, "call *%%rdx")) {
        return ASM_X64_DBT_TRAMPOLINE;
    }
    return ASM_GENERIC;
}

static Node *parse_asm_operands(int *out_count) {
    Node *head;
    Node *tail;
    Node *op;
    int count;

    head = NULL;
    tail = NULL;
    count = 0;

    while (lex_tok != TK_COLON && lex_tok != TK_RPAREN && lex_tok != TK_EOF) {
        if (count > 0) expect(TK_COMMA);

        /* Optional GNU symbolic operand name: [name] "r" (expr). */
        if (lex_tok == TK_LBRACK) {
            next();
            if (lex_tok == TK_IDENT) next();
            expect(TK_RBRACK);
        }

        if (lex_tok != TK_STRING) p_error("expected asm constraint");
        parse_string_literal();
        expect(TK_LPAREN);
        op = parse_assign();
        expect(TK_RPAREN);
        op->next = NULL;

        if (head == NULL) {
            head = op;
            tail = op;
        } else {
            tail->next = op;
            tail = op;
        }
        count = count + 1;
    }

    *out_count = count;
    return head;
}

static void parse_asm_clobbers(void) {
    while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) {
        if (lex_tok != TK_STRING) p_error("expected asm clobber");
        parse_string_literal();
        if (lex_tok == TK_COMMA) next();
        else break;
    }
}

static Node *parse_gnu_asm_stmt(void) {
    int tmpl_idx;
    char *tmpl;
    Node *outputs;
    Node *inputs;
    int nout;
    int nin;

    if (!is_gnu_asm_ident()) p_error("expected asm");
    next();
    while (lex_tok == TK_VOLATILE || is_gnu_qual_ident()) next();

    expect(TK_LPAREN);
    if (lex_tok != TK_STRING) p_error("expected asm template");
    tmpl_idx = parse_string_literal();
    tmpl = lex_strpool + lex_str_off[tmpl_idx];

    outputs = NULL;
    inputs = NULL;
    nout = 0;
    nin = 0;

    if (lex_tok == TK_COLON) {
        next();
        if (lex_tok != TK_COLON && lex_tok != TK_RPAREN) {
            outputs = parse_asm_operands(&nout);
        }
        if (lex_tok == TK_COLON) {
            next();
            if (lex_tok != TK_COLON && lex_tok != TK_RPAREN) {
                inputs = parse_asm_operands(&nin);
            }
            if (lex_tok == TK_COLON) {
                next();
                parse_asm_clobbers();
            }
        }
    }

    expect(TK_RPAREN);
    (void)nout;
    return nd_asm(tmpl, outputs, inputs, classify_gnu_asm(tmpl), nin);
}

/* --- Expression parser (operator precedence climbing) --- */

static Node *parse_compound_literal_expr(int ty, int arr_count);

/* A type name in an expression -- _Alignof's operand, a _Generic
 * association: its C type, in ps_type_cty.  Array dimensions after it
 * are read and left out (neither use needs them). */
static int ps_type_name_cty(void) {
    int ty;
    ty = parse_type();
    ty = ps_abstract_fnptr(ty);
    skip_decl_qualifiers();
    while (lex_tok == TK_LBRACK) {
        while (lex_tok != TK_RBRACK && lex_tok != TK_EOF) next();
        expect(TK_RBRACK);
    }
    return ps_type_cty;
}

/* _Alignof ( type-name ) (6.5.3.4p3), size_t; and GNU's __alignof__,
 * which also takes an expression. */
static Node *ps_parse_alignof(void) {
    Node *e;
    int t;
    int gnu;
    gnu = lex_str[1] == 95;     /* __alignof__: GNU's, which also takes an expression */
    next();
    expect(TK_LPAREN);
    if (is_type()) {
        t = ps_type_name_cty();
    } else {
        if (!gnu) p_error("_Alignof requires a type name");
        e = parse_expr();
        sema_expr(e);
        t = e->cty;
    }
    expect(TK_RPAREN);
    return nd_size(ct_alignof(t));
}

/* _Generic ( assignment-expression , generic-assoc-list ) (6.5.1.1):
 * the association whose type is compatible with the controlling
 * expression's, after lvalue conversion -- which is typed, not
 * evaluated -- or the default.  The others are read and dropped. */
static Node *ps_parse_generic(void) {
    Node *ctrl;
    Node *e;
    Node *chosen;
    Node *deflt;
    int ct;
    int at;
    int seen[64];
    int nseen;
    int k;
    next();
    expect(TK_LPAREN);
    nseen = 0;
    ctrl = parse_assign();
    sema_expr(ctrl);
    ct = sema_vty(ctrl);
    chosen = NULL;
    deflt = NULL;
    while (lex_tok == TK_COMMA) {
        next();
        if (lex_tok == TK_DEFAULT) {
            next();
            expect(TK_COLON);
            e = parse_assign();
            if (deflt) p_error("duplicate default generic association");
            deflt = e;
        } else {
            at = ps_type_name_cty();
            /* no two associations of compatible types (6.5.1.1p2) */
            k = 0;
            while (k < nseen) {
                if (sema_compat(seen[k], at)) p_error("type in generic association compatible with a previous one");
                k = k + 1;
            }
            if (nseen < 64) { seen[nseen] = at; nseen = nseen + 1; }
            expect(TK_COLON);
            e = parse_assign();
            if (chosen == NULL && sema_compat(at, ct)) chosen = e;
        }
    }
    expect(TK_RPAREN);
    if (chosen == NULL) chosen = deflt;
    if (chosen == NULL) p_error("controlling expression type not compatible with any generic association type");
    return chosen;
}

/* An integer literal, marked when it was written in hex or octal:
 * clang does not warn when one such fills a signed type's width
 * (signed char c = 0377), as it does for the same value in decimal. */
static Node *ps_num_spelled(Node *n, int hexoct) {
    if (hexoct) n->attr = n->attr | AT_HEXOCT;
    return n;
}

/* The C11 primaries that look like identifiers: _Alignof, _Generic, and
 * __func__ (6.4.2.2), a string of the function's name -- each was an
 * undeclared identifier.  NULL when the token is none of them. */
static Node *ps_c11_primary(void) {
    if (lex_tok != TK_IDENT || lex_str[0] != 95) return NULL;
    if (strcmp(lex_str, "_Alignof") == 0 || strcmp(lex_str, "__alignof__") == 0 ||
        strcmp(lex_str, "__alignof") == 0)
        return ps_parse_alignof();
    if (strcmp(lex_str, "_Generic") == 0) return ps_parse_generic();
    if ((strcmp(lex_str, "__func__") == 0 || strcmp(lex_str, "__FUNCTION__") == 0) &&
        ps_in_body && ps_cur_func) {
        pp_emit_string_token(ps_cur_func);   /* now a string literal token */
        return parse_primary();
    }
    return NULL;
}

static Node *parse_primary(void) {
    int cast_cty;
    int cast_vol;
    int cast_rowptr;
    Node *n;
    int v;
    int ty;
    int arr_count;
    char nm[256];
    Node *head;
    Node *tail;
    Node *arg;
    int nargs;
    int li;
    int gi;
    int ci;
    int i;

    n = ps_c11_primary();
    if (n) return n;

    /* Number literal */
    if (lex_tok == TK_NUM) {
        int v_hi;
        int v_ll;
        int v_u;
        int v_hexoct;
        int v_macro;
        int v_l;
        v    = lex_val;
        v_hi = lex_val_hi;
        v_ll = lex_val_ll;
        v_l  = lex_val_l;
        v_u  = lex_val_u;
        v_hexoct = lex_val_hexoct;
        v_macro = lex_val_macro;
        next();
        /* Promote to long long when the literal had an LL/LLU suffix or
         * its high 32 bits aren't zero — otherwise treat as int (and let
         * downstream usual-arithmetic-conversions handle widening).  An
         * unsigned-only suffix on a value < 2^31 stays a 32-bit int; the
         * subset doesn't distinguish unsigned int from int at the ND_NUM
         * level. */
        if (v_ll || v_hi != 0) {
            int t;
            t = TY_LLONG;
            /* past long long's range only unsigned long long holds it
             * (6.4.4.1p5): 0xFFFFFFFFFFFFFFFF was a signed -1 */
            if (v_u || v_hi < 0) t = t | TY_UNSIGNED;
            return ps_num_spelled(nd_num64(v, v_hi, t), v_hexoct);
        }
        /* Unsuffixed decimal that does not fit int goes to long long
         * (C11 6.4.4.1); it is never unsigned int.  Hex/octal that does
         * not fit int becomes unsigned int.  2147483648 stored in a
         * 32-bit int looks negative, which is how the hex case was
         * detected — and why decimal 2147483648 was typed unsigned
         * (GitHub issue 40).  SQLite's `db->flags |= 0x80000000` is the
         * hex path: as a signed int it sign-extended into the high word
         * of the 64-bit flags. */
        if (!v_u && !v_hexoct && !v_macro && v < 0)
            return nd_num64(v, 0, TY_LLONG);
        {
            Node *nn;
            nn = nd_num(v);
            /* negative here: hex or octal past INT_MAX, unsigned int --
             * but not a #define's value, injected as one token, which
             * is the int it was (`#define N -2000` was 4294965296 and
             * N / 2 was 2147482648) */
            if (v_u || (v < 0 && !v_macro)) nn->ty = TY_INT | TY_UNSIGNED;
            /* 1L is a long and 1UL an unsigned long: the size of int
             * here, but other types (6.4.4.1p5), as _Generic sees */
            if (v_l) nn->cty = ct_basic[(nn->ty & TY_UNSIGNED) ? CT_ULONG : CT_LONG];
            return ps_num_spelled(nn, v_hexoct);
        }
    }

    /* Float/double literal */
    if (lex_tok == TK_FNUM) {
        n = nd_fnum(lex_val, lex_fval_hi, lex_fty);
        next();
        return n;
    }

    /* Character literal */
    if (lex_tok == TK_CHARLIT) {
        v = lex_val;
        i = lex_chr_prefix;
        next();
        n = nd_num(v);
        /* L'x' is a wchar_t, U'x' a char32_t (unsigned int, as clang's
         * target has them), u'x' a char16_t (6.4.4.4p2) */
        if (i == 76 || i == 85) n->ty = TY_INT | TY_UNSIGNED;
        else if (i == 117) n->ty = TY_SHORT | TY_UNSIGNED;
        return n;
    }

    /* String literal */
    if (lex_tok == TK_STRING) {
        v = parse_string_literal();
        n = nd_string(v);
        /* a wide one points at its code units (6.4.5p6) */
        if (lex_str_width[v] == 4) n->ty = TY_PTR + (TY_INT | TY_UNSIGNED);
        else if (lex_str_width[v] == 2) n->ty = TY_PTR + (TY_SHORT | TY_UNSIGNED);
        return n;
    }

    /* Identifier: variable, enum constant, function call, or function ref */
    if (lex_tok == TK_IDENT) {
        memcpy(nm, lex_str, lex_slen + 1);
        next();

        /* Check local variable first (enables fn ptr calls via postfix) */
        li = find_local(nm);
        if (li >= 0) {
            if (ps_lstatic[li]) {
                n = nd_var(ps_lsname[li], 0, ps_ltype[li]);
                n->cty = ps_lcty[li];
                ps_var_vol(n, ps_ltype[li], ps_larr[li], ps_lvolb[li], ps_lvolp[li]);
                n->is_local = 0;
                n->is_array = ps_larr[li];
                n->arr_cols = ps_lcols[li];
                n->arr_sub = ps_lsub[li];
            } else {
                n = nd_var(nm, ps_loff[li], ps_ltype[li]);
                ps_var_vol(n, ps_ltype[li], ps_larr[li], ps_lvolb[li], ps_lvolp[li]);
                ps_bind_var(n, li);
            }
            /* Locals are popped before sema, so the signature has to
             * live on the node (GitHub issue 41). val is unused on VAR. */
            if (ps_lfpbase[li] >= 0) {
                n->is_fnptr = 1;
                n->val = ps_lfpbase[li];
                n->nparams = ps_lfpn[li];
                n->val_hi = ps_lfpn[li];
            }
            return n;
        }

        /* Global variable */
        gi = find_global(nm);
        if (gi >= 0) {
            n = nd_var(nm, 0, ps_gtype[gi]);
            n->cty = ps_gcty[gi];
            ps_var_vol(n, ps_gtype[gi], ps_gsize[gi] > 0, ps_gvolb[gi], ps_gvolp[gi]);
            n->is_local = 0;
            n->is_array = (ps_gsize[gi] > 0) ? 1 : 0;
            n->arr_cols = ps_gcols[gi];
            n->arr_sub = ps_gsub[gi];
            if (ps_gfpbase[gi] >= 0) {
                n->is_fnptr = 1;
                n->val = ps_gfpbase[gi];
                n->nparams = ps_gfpn[gi];
                n->val_hi = ps_gfpn[gi];
            }
            return n;
        }

        /* Enum constant */
        ci = find_const(nm);
        if (ci >= 0) {
            return nd_num(ps_cval[ci]);
        }

        /* va_start(ap, last) */
        if (strcmp(nm, "va_start") == 0 || strcmp(nm, "__builtin_va_start") == 0) {
            expect(TK_LPAREN);
            arg = parse_assign();
            expect(TK_COMMA);
            parse_assign();
            expect(TK_RPAREN);
            n = nd_new(ND_VA_START);
            n->lhs = arg;
            n->ty = TY_INT;
            return n;
        }

        /* va_arg(ap, type) */
        if (strcmp(nm, "va_arg") == 0 || strcmp(nm, "__builtin_va_arg") == 0) {
            expect(TK_LPAREN);
            arg = parse_assign();
            expect(TK_COMMA);
            ty = parse_type();
            ty = ps_declarator_stars(ty, ps_type_cty);
            ty = ps_abstract_fnptr(ty);
            skip_decl_qualifiers();
            expect(TK_RPAREN);
            n = nd_new(ND_VA_ARG);
            n->lhs = arg;
            n->ty = ty;
            n->cty = ps_type_cty;   /* va_arg(ap, fnptr_t) was an int */
            ps_rowptr_value(n);
            return n;
        }

        /* va_end(ap) — no-op */
        if (strcmp(nm, "va_end") == 0 || strcmp(nm, "__builtin_va_end") == 0) {
            expect(TK_LPAREN);
            parse_assign();
            expect(TK_RPAREN);
            return nd_num(0);
        }

        /* va_copy(dst, src) — assignment */
        if (strcmp(nm, "va_copy") == 0) {
            expect(TK_LPAREN);
            n = parse_assign();
            expect(TK_COMMA);
            arg = parse_assign();
            expect(TK_RPAREN);
            return nd_assign(n, arg);
        }

        /* __builtin_expect(expr, val) — returns expr (hint ignored) */
        if (strcmp(nm, "__builtin_expect") == 0) {
            expect(TK_LPAREN);
            n = parse_assign();
            expect(TK_COMMA);
            parse_assign();  /* discard second arg (the expected value) */
            expect(TK_RPAREN);
            return n;
        }

        /* __builtin_nan[f|l]("..."), __builtin_inf[f|l](),
         * __builtin_huge_val[f|l](): the constants, as clang folds them --
         * runtime/include/math.h's NAN and INFINITY are these, and a call
         * to them links to nothing (selfhost ISSUES-71).  The NaN is the
         * quiet one; a payload string is not read.  long double is double. */
        if (strcmp(nm, "__builtin_nanf") == 0 || strcmp(nm, "__builtin_nan") == 0 || strcmp(nm, "__builtin_nanl") == 0) {
            int f = strcmp(nm, "__builtin_nanf") == 0;
            expect(TK_LPAREN);
            parse_assign();  /* the payload string: not read */
            expect(TK_RPAREN);
            return f ? nd_fnum(0x7FC00000, 0, TY_FLOAT) : nd_fnum(0, 0x7FF80000, TY_DOUBLE);
        }
        if (strcmp(nm, "__builtin_inff") == 0 || strcmp(nm, "__builtin_inf") == 0 || strcmp(nm, "__builtin_infl") == 0 ||
            strcmp(nm, "__builtin_huge_valf") == 0 || strcmp(nm, "__builtin_huge_val") == 0 || strcmp(nm, "__builtin_huge_vall") == 0) {
            int f = strcmp(nm, "__builtin_inff") == 0 || strcmp(nm, "__builtin_huge_valf") == 0;
            expect(TK_LPAREN);
            expect(TK_RPAREN);
            return f ? nd_fnum(0x7F800000, 0, TY_FLOAT) : nd_fnum(0, 0x7FF00000, TY_DOUBLE);
        }

        /* __builtin___clear_cache(begin, end) — no-op in the portable
         * frontend.  AArch64 code that really needs cache maintenance uses
         * the supported inline-asm subset above. */
        if (strcmp(nm, "__builtin___clear_cache") == 0) {
            expect(TK_LPAREN);
            parse_assign();  /* discard begin */
            expect(TK_COMMA);
            parse_assign();  /* discard end */
            expect(TK_RPAREN);
            return nd_num(0);
        }

        /* Direct function call: name(args) */
        if (lex_tok == TK_LPAREN) {
            next();
            head = NULL;
            tail = NULL;
            nargs = 0;
            if (lex_tok != TK_RPAREN) {
                arg = parse_assign();
                nargs = 1;
                head = arg;
                tail = arg;
                while (lex_tok == TK_COMMA) {
                    next();
                    arg = parse_assign();
                    tail->next = arg;
                    tail = arg;
                    nargs = nargs + 1;
                }
            }
            expect(TK_RPAREN);
            /* a function is declared before it is called (6.5.1p2; the
             * implicit int declaration went with C99): sema_cvalof called
             * above its declaration came back an int, its long long cut
             * to 32 bits.  The compiler's own builtins are known by name. */
            n = nd_call(nm, head, nargs);
            if (!is_known_func(nm) && strncmp(nm, "__builtin_", 10) != 0) {
                memcpy(ps_implicit_msg, "call to undeclared function ", 28);
                strncpy(ps_implicit_msg + 28, nm, 200);
                ps_implicit_msg[228] = 0;
                sema_convert_diag(n, "implicit-fn", ps_implicit_msg);
            }
            if (strcmp(nm, "setjmp") == 0 || strcmp(nm, "_setjmp") == 0 ||
                strcmp(nm, "sigsetjmp") == 0 || strcmp(nm, "__sigsetjmp") == 0)
                ps_fn_setjmp = 1;
            n->ty = find_func_type(nm);
            /* __builtin_sqrt[f]: the lowering emits HI_FSQRT, but the call
             * node's type was the implicit int of an undeclared function, so
             * a caller converted the double's bits as an integer -- sqrt(2.0)
             * came back 4.6e18 in libc_x64's cc-x64-compiled math_stubs and
             * every FSQRT the self-hosted dbt-x64 ran was wrong
             * (tools/dbt/ISSUES.md, DBT-20).  The argument is taken as it is:
             * its callers pass a double. */
            if (strcmp(nm, "__builtin_sqrt") == 0) n->ty = TY_DOUBLE;
            else if (strcmp(nm, "__builtin_sqrtf") == 0) n->ty = TY_FLOAT;
            i = 0;
            while (i < ps_nfuncs) {
                if (strcmp(nm, ps_fname[i]) == 0) {
                    if (ps_fretfpbase[i] >= 0) {
                        n->is_fnptr = 1;
                        n->val = ps_fretfpbase[i];
                        n->val_hi = ps_fretfpn[i];
                    }
                    break;
                }
                i = i + 1;
            }
            return n;
        }

        /* Bare function name: load address (for function pointers).
         * Require a prior declaration — otherwise a typo'd local/global
         * silently emits an unresolved relocation. */
        if (is_known_func(nm)) {
            return nd_func_ref(nm);
        }
        pp_where();
        fdputs("error: undeclared identifier '", 2);
        fdputs(nm, 2);
        fdputs("'\n", 2);
        exit(1);
    }

    /* Parenthesized expression, type cast, or GNU statement expression */
    if (lex_tok == TK_LPAREN) {
        next();
        /* GNU statement expression: ( { stmts; final_expr; } )
         * The body is parsed as a regular block (which scopes its
         * locals via saved_nlocals); the trailing expression-stmt is
         * detached and stored as `lhs` so hl_expr can return its value
         * without re-evaluating it.  parse_block leaves us positioned
         * on the token after `}`, where we then expect `)`. */
        if (lex_tok == TK_LBRACE) {
            Node *blk;
            Node *prev;
            Node *last;
            Node *se;
            blk = parse_block();
            if (blk->body == NULL) {
                p_error("empty statement expression");
                expect(TK_RPAREN);
                return nd_num(0);
            }
            /* Find the final statement; unlink it from the body. */
            prev = NULL;
            last = blk->body;
            while (last->next != NULL) { prev = last; last = last->next; }
            if (last->kind != ND_EXPR_STMT || last->lhs == NULL) {
                p_error("statement expression must end with an expression statement");
                expect(TK_RPAREN);
                return nd_num(0);
            }
            if (prev) prev->next = NULL;
            else      blk->body  = NULL;
            se = nd_num(0);
            se->kind = ND_STMT_EXPR;
            se->body = blk;
            se->lhs  = last->lhs;
            se->ty   = last->lhs->ty;
            expect(TK_RPAREN);
            return se;
        }
        if (is_type()) {
            ty = parse_type();
            cast_vol = ps_type_volb;
            arr_count = 0;
            ps_cl_cols = 0;
            if (lex_tok == TK_LBRACK) {
                /* (T[N][M]...): flattened, its rows recorded for the
                 * literal (ps_cl_cols, ps_cl_sub); N may be left to the
                 * initializer */
                next();
                arr_count = -1;
                if (lex_tok != TK_RBRACK) arr_count = parse_const_int();
                expect(TK_RBRACK);
                ps_cl_cols = ps_inner_dims(0, &ps_cl_sub);
                if (arr_count > 0 && ps_cl_cols > 0) arr_count = arr_count * ps_cl_cols;
            }
            ty = ps_abstract_fnptr(ty);   /* (int (**)(sqlite3_vtab *))p */
            cast_rowptr = ps_abs_rowptr;
            cast_cty = ps_type_cty;       /* before the operand runs parse_type again */
            skip_decl_qualifiers();
            expect(TK_RPAREN);
            if (lex_tok == TK_LBRACE) {
                return parse_compound_literal_expr(ty, arr_count);
            }
            if (arr_count != 0) p_error("array cast unsupported");
            n = parse_unary();
            n = nd_cast(n, ty);
            n->cty = cast_cty;
            if (cast_vol && ty_is_ptr(ty)) n->is_volptr = 1;   /* (volatile u8 *)REG */
            ps_abs_rowptr = cast_rowptr;
            ps_rowptr_value(n);         /* (int (*)[4])p: a pointer to rows */
            return n;
        }
        n = parse_expr();
        expect(TK_RPAREN);
        return n;
    }

    /* __builtin_offsetof(type, member) / offsetof(type, member) */
    if (lex_tok == TK_OFFSETOF) {
        next();
        v = parse_offsetof_value();
        return nd_size(v);
    }

    /* sizeof(type_or_expr) / sizeof unary-expression */
    if (lex_tok == TK_SIZEOF) {
        next();
        if (lex_tok != TK_LPAREN) {
            n = parse_unary();
            return ps_sizeof_value(n);
        }
        expect(TK_LPAREN);
        if (is_type()) {
            int sz_ty;
            sz_ty = parse_type();
            if (ps_vla_sizeof_ahead()) return ps_vla_sizeof_type();
            v = ps_sizeof_type_tail(sz_ty, ps_type_arrcount);
            expect(TK_RPAREN);
        } else {
            n = parse_expr();
            expect(TK_RPAREN);
            return ps_sizeof_value(parse_postfix_from(n));
        }
        return nd_size(v);
    }

    p_error("unexpected token in expression");
    return nd_num(0);
}

/* Postfix: handle array subscript p[i], postfix ++/--, member access . and -> */
static Node *parse_postfix_from(Node *n);
static Node *parse_postfix(void) { return parse_postfix_from(parse_primary()); }
/* The postfix operators applied to n, a primary already parsed -- by
 * parse_postfix, and by sizeof, whose "( expression )" is the start of a
 * postfix-expression, not the whole operand: sizeof("abc")[0] is
 * sizeof(("abc")[0]), 1 (C90 6.3.3 / 6.3.2; selfhost ISSUES-84) */
/* A member of a ?: of structures, (t ? a : b).y: the lowering takes a
 * member through its structure's address, and a ?: result has none
 * ("not an lvalue").  With both arms lvalues, the value is that of
 * (*(t ? &a : &b)).y -- one arm's member, read. */
static int ps_is_lvalue_node(Node *x) {
    return x->kind == ND_VAR || x->kind == ND_MEMBER ||
           (x->kind == ND_UNARY && x->op == TK_STAR);
}

static Node *ps_cond_struct_lvalue(Node *n) {
    Node *t;
    if (n->kind != ND_TERNARY || !ty_is_struct(n->ty) || !n->lhs || !n->rhs) return n;
    if (!ps_is_lvalue_node(n->lhs) || !ps_is_lvalue_node(n->rhs)) return n;
    t = nd_ternary(n->cond, nd_unary(TK_AMP, n->lhs), nd_unary(TK_AMP, n->rhs));
    return nd_unary(TK_STAR, t);
}

static Node *parse_postfix_from(Node *n) {
    Node *idx;
    Node *pi;
    Node *ahead;
    Node *atail;
    Node *aarg;
    int anargs;
    int sty;
    int mi;
    char mnm[256];

    while (lex_tok == TK_LBRACK || lex_tok == TK_INC || lex_tok == TK_DEC ||
           lex_tok == TK_DOT || lex_tok == TK_ARROW || lex_tok == TK_LPAREN) {
        if (lex_tok == TK_LPAREN) {
            /* Indirect call through expression: expr(args) */
            next();
            ahead = NULL;
            atail = NULL;
            anargs = 0;
            if (lex_tok != TK_RPAREN) {
                aarg = parse_assign();
                anargs = 1;
                ahead = aarg;
                atail = aarg;
                while (lex_tok == TK_COMMA) {
                    next();
                    aarg = parse_assign();
                    atail->next = aarg;
                    atail = aarg;
                    anargs = anargs + 1;
                }
            }
            expect(TK_RPAREN);
            /* C: (*f)(args) == f(args) for function pointers.  In this
             * subset a function pointer VALUE is int-typed, so the
             * no-op derefs to strip are exactly the STARs whose
             * operand is NOT a real pointer.  A STAR over a pointer
             * operand is a genuine element load (wipes[i] loads the
             * slot; stripping it called the slot's ADDRESS). */
            while (n->kind == ND_UNARY && n->op == TK_STAR &&
                   n->lhs != NULL &&
                   (!ty_is_ptr(n->lhs->ty) ||
                    (n->lhs->kind == ND_MEMBER && n->lhs->is_fnptr)))
                n = n->lhs;   /* a fn-ptr MEMBER is typed as a pointer;
                               * (*pCtx->pFunc->xSFunc)(..) (OP_Function)
                               * loaded the code word the pointer named
                               * and jumped to it */
            n = nd_call_ptr(n, ahead, anargs);
            n->ty = ps_fp_ret_of_callee(n->lhs);
        } else if (lex_tok == TK_LBRACK) {
            next();
            idx = parse_expr();
            expect(TK_RBRACK);
            if (n->is_array && n->arr_cols > 0) {
                /* First index of a 2D array: row address, no load.
                 * arr[i][j] => *((arr + i*cols) + j); the element-size
                 * scaling of both additions stays in codegen. */
                {
                    int row_cols;
                    int row_sub;
                    row_cols = n->arr_cols;
                    row_sub = n->arr_sub;
                    idx = nd_binop(TK_STAR, idx, nd_num(row_cols));
                    n = nd_binop(TK_PLUS, n, idx);
                    n->arr_cols = row_cols;     /* sizeof a[i] is a row, not a pointer */
                    n->arr_sub = row_sub;
                }
            } else if (n->kind == ND_BINOP && n->op == TK_PLUS && n->arr_sub > 0) {
                /* A subscript of a row that is itself an array of rows
                 * (int a[2][3][4]: a[i] and then [j]): again an address,
                 * stepping over the rows below it. */
                {
                    int sub;
                    sub = n->arr_sub;
                    idx = nd_binop(TK_STAR, idx, nd_num(ast_sub_cols[sub]));
                    n = nd_binop(TK_PLUS, n, idx);
                    n->arr_cols = ast_sub_cols[sub];
                    n->arr_sub = ast_sub_next[sub];
                }
            } else if ((n->kind == ND_MEMBER || n->kind == ND_VAR || n->kind == ND_CAST ||
                        n->kind == ND_VA_ARG) && !n->is_array &&
                       n->arr_cols > 0 && ty_is_ptr(n->ty)) {
                /* Pointer-to-array member: p[i] selects row i — scale
                 * by the row width, keep the value a row pointer (no
                 * element load).  arr_cols is kept on the row node so
                 * sizeof(p[i]) reports the row size. */
                {
                    int row_cols;
                    int row_sub;
                    row_cols = n->arr_cols;
                    row_sub = n->arr_sub;
                    idx = nd_binop(TK_STAR, idx, nd_num(row_cols));
                    n = nd_binop(TK_PLUS, n, idx);
                    n->arr_cols = row_cols;
                    n->arr_sub = row_sub;
                }
            } else {
                /* n[idx] → *(n + idx)  — codegen handles pointer arithmetic scaling.
                 * A function-pointer array's slot keeps the signature so
                 * methods[i](0) still converts i64 args (GitHub issue 41). */
                {
                    int fpb;
                    int fpn;
                    fpb = n->is_fnptr ? n->val : -1;
                    fpn = n->is_fnptr ? n->nparams : 0;
                    if (n->kind == ND_CALL) fpn = n->val_hi;
                    n = nd_binop(TK_PLUS, n, idx);
                    n = nd_unary(TK_STAR, n);
                    ps_vla_row(n);
                    if (fpb >= 0) {
                        n->is_fnptr = 1;
                        n->val = fpb;
                        n->nparams = fpn;
                        n->val_hi = fpn;
                    }
                }
            }
        } else if (lex_tok == TK_DOT) {
            next();
            if (lex_tok != TK_IDENT) {
                p_error("expected member name after '.'");
            }
            memcpy(mnm, lex_str, lex_slen + 1);
            next();
            n = ps_cond_struct_lvalue(n);
            sty = n->ty;
            if (!ty_is_struct(sty)) {
                p_error("'.' on non-struct type");
            }
            mi = find_member(sty, mnm);
            if (mi < 0) {
                p_error("undefined struct member");
            }
            n = nd_member(n, stm_off[mi], stm_type[mi], stm_is_arr[mi], stm_arr_size[mi],
                          stm_bit_off[mi], stm_bit_width[mi]);
            n->cty = stm_cty[mi];     /* sema adds the structure's qualifiers */
            if (stm_vol[mi]) ps_var_vol(n, stm_type[mi], stm_is_arr[mi], 1, 0);
            n->offset = stm_fpbase[mi];
            n->nparams = stm_fpn[mi];
            n->is_fnptr = stm_isfp[mi];
            n->arr_cols = stm_arr_cols[mi];
            n->arr_sub = stm_arr_sub[mi];
        } else if (lex_tok == TK_ARROW) {
            next();
            if (lex_tok != TK_IDENT) {
                p_error("expected member name after '->'");
            }
            memcpy(mnm, lex_str, lex_slen + 1);
            next();
            /* Desugar p->m to (*p).m */
            if (!ty_is_ptr(n->ty)) {
                p_error("'->' on non-pointer type");
            }
            sty = ty_deref(n->ty);
            if (!ty_is_struct(sty)) {
                p_error("'->' on pointer to non-struct");
            }
            /* Dereference: *p gives struct, but we don't load — create ND_UNARY TK_STAR */
            n = nd_unary(TK_STAR, n);
            mi = find_member(sty, mnm);
            if (mi < 0) {
                p_error("undefined struct member");
            }
            n = nd_member(n, stm_off[mi], stm_type[mi], stm_is_arr[mi], stm_arr_size[mi],
                          stm_bit_off[mi], stm_bit_width[mi]);
            n->cty = stm_cty[mi];     /* sema adds the structure's qualifiers */
            if (stm_vol[mi]) ps_var_vol(n, stm_type[mi], stm_is_arr[mi], 1, 0);
            n->offset = stm_fpbase[mi];
            n->nparams = stm_fpn[mi];
            n->is_fnptr = stm_isfp[mi];
            n->arr_cols = stm_arr_cols[mi];
            n->arr_sub = stm_arr_sub[mi];
        } else if (lex_tok == TK_INC) {
            next();
            pi = nd_new(ND_POST_INC);
            pi->lhs = n;
            pi->ty = n->ty;
            n = pi;
        } else {
            next();
            pi = nd_new(ND_POST_DEC);
            pi->lhs = n;
            pi->ty = n->ty;
            n = pi;
        }
    }
    return n;
}

/* &a[i] on a 2-D array or a pointer to one: the row's address, which
 * the sum a[i] already is, typed as a pointer to the row (6.5.3.2p3) --
 * as it stood, the sum's own type, the row, decayed to a pointer to its
 * first element, and int (*q)[3] = &m[i] was refused as incompatible. */
static Node *ps_row_address(Node *n) {
    int t;
    if (n->arr_cols <= 0 || n->lhs == NULL || n->lhs->cty <= 0) return n;
    t = ct_decay(n->lhs->cty);
    if (!ct_is_ptr(t)) return n;
    t = ct_baseof(t);
    while (ct_is_array(t)) t = ct_baseof(t);
    n = nd_cast(n, n->ty);
    n->cty = ct_ptr(ps_rows_cty(t, n->lhs->arr_cols, n->lhs->arr_sub));
    return n;
}

static Node *parse_unary(void) {
    Node *n;

    if (lex_tok == TK_PLUS) {
        /* unary plus: the operand, promoted (6.5.3.3p2) -- a char or
         * short operand becomes int, so sizeof(+c) is int's size */
        next();
        n = parse_unary();
        if (nd_bf_promoted(n) >= 0)
            n = nd_cast(n, nd_bf_promoted(n));
        else if (!ty_is_ptr(n->ty) && ((n->ty & TY_BASE_MASK) == TY_CHAR ||
                                       (n->ty & TY_BASE_MASK) == TY_SHORT))
            n = nd_cast(n, TY_INT);
        return n;
    }
    if (lex_tok == TK_MINUS) {
        next();
        n = parse_unary();
        return nd_unary(TK_MINUS, n);
    }
    if (lex_tok == TK_BANG) {
        next();
        n = parse_unary();
        return nd_unary(TK_BANG, n);
    }
    /* Bitwise NOT */
    if (lex_tok == TK_TILDE) {
        next();
        n = parse_unary();
        return nd_unary(TK_TILDE, n);
    }
    /* Prefix ++ (desugar to compound assign) */
    if (lex_tok == TK_INC) {
        next();
        n = parse_unary();
        return nd_comp_assign(TK_PLUS, n, nd_num(1));
    }
    /* Prefix -- (desugar to compound assign) */
    if (lex_tok == TK_DEC) {
        next();
        n = parse_unary();
        return nd_comp_assign(TK_MINUS, n, nd_num(1));
    }
    /* Dereference */
    if (lex_tok == TK_STAR) {
        int dfpb;
        int dfpn;
        next();
        n = parse_unary();
        /* *f on a function designator is f again (6.5.3.2p4, which then
         * decays back, 6.3.2.1p4): it loaded through the code address,
         * and *f called as a pointer faulted. */
        if (n->kind == ND_FUNC_REF) return n;
        dfpb = n->is_fnptr ? n->val : -1;
        dfpn = n->is_fnptr ? n->nparams : 0;
        n = nd_unary(TK_STAR, n);
        /* (*tab)(x), the deref twin of the tab[0](x) case the subscript
         * path already handles (GitHub issue 70). */
        if (dfpb >= 0) {
            n->is_fnptr = 1;
            n->val = dfpb;
            n->nparams = dfpn;
            n->val_hi = dfpn;
        }
        return n;
    }
    /* Address-of */
    if (lex_tok == TK_AMP) {
        next();
        n = parse_unary();
        if (n->kind == ND_MEMBER && n->bit_width > 0) {
            p_error("cannot take address of bit-field");
        }
        if (n->kind == ND_VAR && n->is_local && find_local(n->name) >= 0 &&
            ps_lreg[find_local(n->name)])
            p_error("address of register variable requested");
        /* &f is the function's address, the value f decays to (6.5.3.2p3);
         * it was refused as "not an lvalue". */
        if (n->kind == ND_FUNC_REF) {
            n->attr = n->attr | AT_AMPF;    /* clang's always-true warning spares &f */
            return n;
        }
        /* &arr2d[i]: the first subscript of a 2D array already yields
         * the row ADDRESS (an ADD node, not an lvalue).  C's pointer-
         * to-row has the same numeric value, so the node stands as-is
         * (m_menu's fread(&savegamestrings[i], ...)).  Plain &(a+b)
         * is not legal C, so this only fires for that shape. */
        if (n->kind == ND_BINOP && n->op == TK_PLUS) {
            return ps_row_address(n);
        }
        return nd_unary(TK_AMP, n);
    }
    return parse_postfix();
}

static Node *parse_multiplicative(void) {
    Node *n;
    int op;

    n = parse_unary();
    while (lex_tok == TK_STAR || lex_tok == TK_SLASH || lex_tok == TK_PERCENT) {
        op = lex_tok;
        next();
        n = nd_binop(op, n, parse_unary());
    }
    return n;
}

static Node *parse_additive(void) {
    Node *n;
    int op;

    n = parse_multiplicative();
    while (lex_tok == TK_PLUS || lex_tok == TK_MINUS) {
        op = lex_tok;
        next();
        n = nd_binop(op, n, parse_multiplicative());
    }
    return n;
}

static Node *parse_shift(void) {
    Node *n;
    int op;

    n = parse_additive();
    while (lex_tok == TK_LSHIFT || lex_tok == TK_RSHIFT) {
        op = lex_tok;
        next();
        n = nd_binop(op, n, parse_additive());
    }
    return n;
}

static Node *parse_relational(void) {
    Node *n;
    int op;

    n = parse_shift();
    while (lex_tok == TK_LT || lex_tok == TK_GT || lex_tok == TK_LE || lex_tok == TK_GE) {
        op = lex_tok;
        next();
        n = nd_binop(op, n, parse_shift());
    }
    return n;
}

static Node *parse_equality(void) {
    Node *n;
    int op;

    n = parse_relational();
    while (lex_tok == TK_EQ || lex_tok == TK_NE) {
        op = lex_tok;
        next();
        n = nd_binop(op, n, parse_relational());
    }
    return n;
}

static Node *parse_band(void) {
    Node *n;

    n = parse_equality();
    while (lex_tok == TK_AMP) {
        next();
        n = nd_binop(TK_AMP, n, parse_equality());
    }
    return n;
}

static Node *parse_bxor(void) {
    Node *n;

    n = parse_band();
    while (lex_tok == TK_CARET) {
        next();
        n = nd_binop(TK_CARET, n, parse_band());
    }
    return n;
}

static Node *parse_bor(void) {
    Node *n;

    n = parse_bxor();
    while (lex_tok == TK_PIPE) {
        next();
        n = nd_binop(TK_PIPE, n, parse_bxor());
    }
    return n;
}

static Node *parse_land(void) {
    Node *n;

    n = parse_bor();
    while (lex_tok == TK_LAND) {
        next();
        n = nd_binop(TK_LAND, n, parse_bor());
    }
    return n;
}

static Node *parse_lor(void) {
    Node *n;

    n = parse_land();
    while (lex_tok == TK_LOR) {
        next();
        n = nd_binop(TK_LOR, n, parse_land());
    }
    return n;
}

static Node *parse_conditional(void) {
    Node *n;
    Node *then_e;
    Node *else_e;

    n = parse_lor();
    if (lex_tok == TK_QMARK) {
        next();
        then_e = parse_expr();
        expect(TK_COLON);
        else_e = parse_conditional();
        n = nd_ternary(n, then_e, else_e);
    }
    return n;
}

static Node *parse_assign(void) {
    Node *n;
    int op;

    n = parse_conditional();
    if (lex_tok == TK_ASSIGN) {
        next();
        n = nd_assign(n, parse_assign());
        n->op = TK_ASSIGN;      /* the program's own, not an initializer */
        return n;
    }
    /* Compound assignment operators */
    op = 0;
    if (lex_tok == TK_PLUSEQ)    { op = TK_PLUS;    }
    if (lex_tok == TK_MINUSEQ)   { op = TK_MINUS;   }
    if (lex_tok == TK_STAREQ)    { op = TK_STAR;    }
    if (lex_tok == TK_SLASHEQ)   { op = TK_SLASH;   }
    if (lex_tok == TK_PERCENTEQ) { op = TK_PERCENT; }
    if (lex_tok == TK_AMPEQ)     { op = TK_AMP;     }
    if (lex_tok == TK_PIPEEQ)    { op = TK_PIPE;    }
    if (lex_tok == TK_CARETEQ)   { op = TK_CARET;   }
    if (lex_tok == TK_LSHIFTEQ)  { op = TK_LSHIFT;  }
    if (lex_tok == TK_RSHIFTEQ)  { op = TK_RSHIFT;  }
    if (op) {
        next();
        return nd_comp_assign(op, n, parse_assign());
    }
    return n;
}

static Node *parse_expr(void) {
    Node *n;

    n = parse_assign();
    while (lex_tok == TK_COMMA) {
        next();
        n = nd_comma(n, parse_assign());
    }
    return n;
}

static char *ps_li_name;
static int ps_li_off;
static int ps_li_base_ty;
static int ps_li_base_is_array;
static Node *ps_li_head;
static Node *ps_li_tail;

static void local_init_begin(char *nm, int off, int base_ty, int base_is_array) {
    ps_li_name = nm;
    ps_li_off = off;
    ps_li_slot = 0;
    if (off != 0 && ps_nlocals > 0 && ps_loff[ps_nlocals - 1] == off)
        ps_li_slot = ps_lslot[ps_nlocals - 1];
    ps_li_base_ty = base_ty;
    ps_li_base_is_array = base_is_array;
    ps_li_head = NULL;
    ps_li_tail = NULL;
    /* the target's C type: the local's, or -- an array is allocated
     * after its initializer -- the declaration's, as add_local_array
     * will take it */
    if (ps_li_slot != 0) ps_init_cty = ps_lcty[ps_nlocals - 1];
    else                 ps_init_cty = ps_type_cty;
}

static void ps_make_compound_literal_name(char *buf) {
    int i;
    int v;
    int d;
    char digits[12];

    buf[0] = '_';
    buf[1] = '_';
    buf[2] = 'c';
    buf[3] = 'l';
    buf[4] = 'i';
    buf[5] = 't';
    buf[6] = '.';
    i = 7;
    v = ps_comp_lit_id;
    ps_comp_lit_id = ps_comp_lit_id + 1;
    if (v == 0) {
        buf[i] = '0';
        i = i + 1;
    } else {
        d = 0;
        while (v > 0) {
            digits[d] = '0' + (v % 10);
            d = d + 1;
            v = v / 10;
        }
        while (d > 0) {
            d = d - 1;
            buf[i] = digits[d];
            i = i + 1;
        }
    }
    buf[i] = 0;
}

static void local_init_append(Node *stmt) {
    if (ps_li_head == NULL) {
        ps_li_head = stmt;
        ps_li_tail = stmt;
    } else {
        ps_li_tail->next = stmt;
        ps_li_tail = stmt;
    }
}

static Node *local_init_base(void) {
    Node *n;
    n = nd_var(ps_li_name, ps_li_off, ps_li_base_ty);
    n->is_local = 1;
    n->is_array = ps_li_base_is_array;
    n->slot_id = ps_li_slot;
    return n;
}

static Node *local_init_lvalue(int rel_off, int target_ty, int target_arr_count) {
    Node *n;
    int arr_sz;
    arr_sz = 0;
    if (rel_off == 0 && !ps_li_base_is_array && !ty_is_struct(ps_li_base_ty))
        return local_init_base();
    if (target_arr_count != 0 && ty_is_ptr(target_ty))
        arr_sz = target_arr_count * ty_size(ty_deref(target_ty));
    n = local_init_base();
    return nd_member(n, rel_off, target_ty, target_arr_count != 0, arr_sz, 0, 0);
}

/* A bit-field member mi of a local's initializer, the structure at
 * rel_off in it: a store through the member, which the lowering does
 * as a bit-field's (the bytes were zeroed before). */
static void local_init_bitfield(int rel_off, int mi) {
    Node *m;
    Node *e;
    int br;
    br = lex_tok == TK_LBRACE;
    if (br) next();
    e = parse_assign();
    if (br) {
        if (lex_tok == TK_COMMA) next();
        expect(TK_RBRACE);
    }
    m = nd_member(local_init_base(), rel_off + stm_off[mi], stm_type[mi], 0, 0,
                  stm_bit_off[mi], stm_bit_width[mi]);
    m->cty = stm_cty[mi] > 0 ? ct_unqual(stm_cty[mi]) : 0;
    m = nd_assign(m, e);
    if (stm_cty[mi] > 0) m->attr = AT_INIT;    /* graded at the field's width */
    local_init_append(nd_expr_stmt(m));
}

static void local_init_emit_assign(int rel_off, int target_ty, Node *rhs) {
    Node *a;
    a = local_init_lvalue(rel_off, target_ty, 0);
    a = nd_assign(a, rhs);
    a = nd_expr_stmt(a);
    local_init_append(a);
}

static void local_init_zero_at(int ty, int arr_count, int rel_off) {
    int i;
    int elem_ty;
    int si;
    int nf;
    int mi;
    int field_ty;
    int field_arr_count;
    int total;
    Node *a;
    Node *args;

    /* A big region (a whole array or struct of 32+ bytes) is one
     * memset call, not one store per scalar: regal's
     * `ReconcileResult result = {0};` is 16KB and became 4,100 stores,
     * each on a far frame slot (lui+addi+add+stw). */
    total = ty_size(ty);
    if (arr_count != 0 && ty_is_ptr(ty)) total = arr_count * ty_size(ty_deref(ty));
    if (total >= 32) {
        a = local_init_lvalue(rel_off, TY_CHAR, 0);
        args = nd_unary(TK_AMP, a);
        args->next = nd_num(0);
        args->next->next = nd_num(total);
        a = nd_call("memset", args, 3);
        local_init_append(nd_expr_stmt(a));
        return;
    }
    if (arr_count != 0 && ty_is_ptr(ty)) {
        elem_ty = ty_deref(ty);
        i = 0;
        while (i < arr_count) {
            local_init_zero_at(elem_ty, 0, rel_off + (i * ty_size(elem_ty)));
            i = i + 1;
        }
        return;
    }
    if (ty_is_struct(ty)) {
        si = ty_struct_idx(ty);
        nf = st_nfields[si];
        i = 0;
        while (i < nf) {
            mi = struct_field_nth_idx(si, i);
            if (mi >= 0) {
                if (!struct_member_is_flexible_array(mi)) {
                    field_ty = stm_type[mi];
                    field_arr_count = struct_member_array_count(mi);
                    local_init_zero_at(field_ty, field_arr_count, rel_off + stm_off[mi]);
                }
            }
            i = i + 1;
            if (st_is_union[si]) break;
        }
        return;
    }
    local_init_emit_assign(rel_off, ty, nd_num(0));
}

static void local_init_patch_offsets(Node *n, char *nm, int off) {
    if (n == NULL) return;
    if (n->kind == ND_VAR && n->is_local && n->offset == 0 &&
        strcmp(n->name, nm) == 0) {
        n->offset = off;
        if (ps_nlocals > 0) n->slot_id = ps_lslot[ps_nlocals - 1];
    }
    local_init_patch_offsets(n->lhs, nm, off);
    local_init_patch_offsets(n->rhs, nm, off);
    local_init_patch_offsets(n->cond, nm, off);
    local_init_patch_offsets(n->body, nm, off);
    local_init_patch_offsets(n->init, nm, off);
    local_init_patch_offsets(n->step, nm, off);
    local_init_patch_offsets(n->els, nm, off);
    local_init_patch_offsets(n->args, nm, off);
    local_init_patch_offsets(n->next, nm, off);
}

static void parse_local_init_value_at(int ty, int arr_count, int rel_off);
static int parse_local_init_array_at(int elem_ty, int count, int rel_off);
static void parse_local_init_struct_at(int ty, int rel_off);

static int parse_local_init_array_at(int elem_ty, int count, int rel_off) {
    int i;
    int max_i;
    int elem_sz;
    int sp_idx;
    int slen;
    char *sp;
    int rel;
    int target_ty;
    int target_arr_count;
    int root;
    int has_brace;
    int ec;

    elem_sz = ty_size(elem_ty);
    ec = ps_init_elem_cty(ps_init_take());
    if ((elem_ty & TY_BASE_MASK) == TY_CHAR && lex_tok == TK_STRING) {
        sp_idx = parse_string_literal();
        slen = lex_str_len[sp_idx];
        ps_init_string_fits(sp_idx, count);
        if (count < 0) count = slen + 1;
        sp = lex_strpool + lex_str_off[sp_idx];
        i = 0;
        while (i < slen && i < count) {
            local_init_emit_assign(rel_off + i, elem_ty, nd_num(sp[i] & 255));
            i = i + 1;
        }
        return count;
    }
    /* See parse_global_init_array_at for the brace-elision rule. */
    has_brace = (lex_tok == TK_LBRACE);
    if (has_brace) next();
    i = 0;
    max_i = 0;
    while (lex_tok != TK_EOF) {
        if (lex_tok == TK_RBRACE) break;
        if (!has_brace && count >= 0 && i >= count) break;
        if (i > 0) {
            if (has_brace) {
                expect(TK_COMMA);
                if (lex_tok == TK_RBRACE) break;
            } else {
                if (lex_tok != TK_COMMA) break;
                if (ps_comma_then_designator()) break;
                expect(TK_COMMA);
            }
        }
        if (lex_tok == TK_LBRACK) {
            ps_init_cty = ec > 0 ? ct_array(ec, -1) : 0;
            parse_global_init_designator(elem_ty + TY_PTR, count, &rel,
                                         &target_ty, &target_arr_count, &root);
            expect(TK_ASSIGN);
            parse_local_init_value_at(target_ty, target_arr_count, rel_off + rel);
            if (root >= 0) {
                i = root + 1;
                if (i > max_i) max_i = i;
            }
        } else {
            if (has_brace && count >= 0 && i >= count) p_error("too many initializers");
            ps_init_cty = ec;
            parse_local_init_value_at(elem_ty, 0, rel_off + (i * elem_sz));
            i = i + 1;
            if (i > max_i) max_i = i;
        }
    }
    if (has_brace) {
        if (lex_tok == TK_COMMA) next();
        expect(TK_RBRACE);
    }
    if (count < 0) count = max_i;
    return count;
}

static void ps_local_init_rows(int elem_ty, int cols, int sub, int rel_off);

/* The inside of a braced local initializer for an array of rows, its
 * `{` read: cols elements elem_ty in all (-1: as many rows as it has),
 * rows shaped by sub (ast_sub_cols), at rel_off.  Positions are kept in
 * elements, as parse_global_init_array2d_at keeps them: a designator
 * (by the array's C type) moves past the subobject it names and the
 * next initializer continues there (6.7.9p17).  Returns the rows. */
static int ps_local_init_rows_in(int elem_ty, int cols, int sub, int rel_off) {
    int pos;
    int max_pos;
    int first;
    int rowc;
    int rsub;
    int nrows;
    int esz;
    int rel;
    int tty;
    int tac;
    int root;
    int agg;
    int in_row;
    rowc = ast_sub_cols[sub];
    rsub = ast_sub_next[sub];
    nrows = cols >= 0 ? cols / rowc : -1;
    esz = ty_size(elem_ty);
    agg = ct_array(ps_rows_cty(ps_obj_cty(elem_ty), rowc, rsub), nrows);
    pos = 0;
    max_pos = 0;
    first = 1;
    while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
        if (!first) {
            expect(TK_COMMA);
            if (lex_tok == TK_RBRACE) break;
        }
        first = 0;
        in_row = pos % rowc;
        if (lex_tok == TK_LBRACK) {
            ps_init_cty = agg;
            parse_global_init_designator(elem_ty + TY_PTR, cols, &rel, &tty, &tac, &root);
            expect(TK_ASSIGN);
            parse_local_init_value_at(tty, tac, rel_off + rel);
            pos = (rel + (tac > 0 ? tac : 1) * esz) / esz;
        } else if (in_row != 0 && rsub > 0 && lex_tok == TK_LBRACE &&
                   in_row % ast_sub_cols[rsub] == 0) {
            ps_local_init_rows(elem_ty, ast_sub_cols[rsub], ast_sub_next[rsub], rel_off + pos * esz);
            pos = pos + ast_sub_cols[rsub];
        } else if (in_row != 0) {
            parse_local_init_array_at(elem_ty, rowc - in_row, rel_off + pos * esz);
            pos = pos + rowc - in_row;
        } else {
            if (nrows >= 0 && pos / rowc >= nrows) p_error("too many initializers");
            ps_local_init_rows(elem_ty, rowc, rsub, rel_off + pos * esz);
            pos = pos + rowc;
        }
        if (pos > max_pos) max_pos = pos;
    }
    if (lex_tok == TK_COMMA) next();
    expect(TK_RBRACE);
    return (max_pos + rowc - 1) / rowc;
}

/* One row of a local multi-dimensional array: cols elements elem_ty
 * shaped by sub, at rel_off.  In braces and itself an array of rows,
 * read as one; otherwise cols elements, braced or flat (6.7.9p20), as a
 * 2-D array's row. */
static void ps_local_init_rows(int elem_ty, int cols, int sub, int rel_off) {
    if (sub <= 0 || lex_tok != TK_LBRACE) {
        parse_local_init_array_at(elem_ty, cols, rel_off);
        return;
    }
    next();
    ps_local_init_rows_in(elem_ty, cols, sub, rel_off);
}

static void parse_local_init_struct_at(int ty, int rel_off) {
    int si;
    int nf;
    int i;
    int mi;
    int field_ty;
    int field_arr_count;
    int has_brace;
    int rel;
    int target_ty;
    int target_arr_count;
    int root;
    int sc;

    si = ty_struct_idx(ty);
    nf = st_nfields[si];
    sc = ps_init_take();
    has_brace = (lex_tok == TK_LBRACE);
    if (has_brace) next();
    i = 0;
    while (lex_tok != TK_EOF) {
        if (has_brace && lex_tok == TK_RBRACE) break;
        if (!has_brace && i >= nf) break;
        if (i > 0) {
            if (has_brace) {
                expect(TK_COMMA);
                if (lex_tok == TK_RBRACE) break;
            } else {
                if (lex_tok != TK_COMMA) break;
                expect(TK_COMMA);
            }
        }
        if (lex_tok == TK_DOT) {
            ps_desig_bf = -1;
            ps_init_cty = sc;
            parse_global_init_designator(ty, 0, &rel, &target_ty,
                                         &target_arr_count, &root);
            expect(TK_ASSIGN);
            if (ps_desig_bf >= 0) local_init_bitfield(rel_off + rel - stm_off[ps_desig_bf], ps_desig_bf);
            else parse_local_init_value_at(target_ty, target_arr_count, rel_off + rel);
            if (root >= 0) i = root + 1;
            else i = i + 1;
        } else {
            if (i >= nf) p_error("too many initializers");
            mi = struct_field_nth_idx(si, i);
            if (mi < 0) p_error("missing struct field");
            if (struct_member_is_flexible_array(mi))
                p_error("flexible array initializer unsupported");
            if (stm_bit_width[mi] > 0) {
                local_init_bitfield(rel_off, mi);
                i = i + 1;
                continue;
            }
            field_ty = stm_type[mi];
            field_arr_count = struct_member_array_count(mi);
            ps_init_cty = stm_cty[mi];
            parse_local_init_value_at(field_ty, field_arr_count, rel_off + stm_off[mi]);
            i = i + 1;
        }
        if (st_is_union[si]) break;
    }
    if (has_brace) {
        if (lex_tok == TK_COMMA) next();
        expect(TK_RBRACE);
    }
}

static void parse_local_init_value_at(int ty, int arr_count, int rel_off) {
    Node *a;
    int c;
    c = ps_init_take();
    if (ps_cty_rows(c) && ty_is_ptr(ty) && lex_tok == TK_LBRACE) {
        /* an array of arrays in braces (a struct's member, a designated
         * row of a bigger one): its rows, as a declared one's */
        next();
        ps_local_init_rows_in(ty_deref(ty), ps_cty_scalars(c),
                              ps_sub(ps_cty_scalars(ct_baseof(c)), ps_cty_sub(ct_baseof(c))), rel_off);
        return;
    }
    if (arr_count != 0 && ty_is_ptr(ty)) {
        ps_init_cty = c;
        parse_local_init_array_at(ty_deref(ty), arr_count, rel_off);
        return;
    }
    if (ty_is_struct(ty)) {
        ps_init_cty = c;
        parse_local_init_struct_at(ty, rel_off);
        return;
    }
    /* the target typed as C types it, so sema grades the conversion as
     * an assignment's (6.7.9p11); its qualifiers do not make it one */
    a = local_init_lvalue(rel_off, ty, 0);
    a = nd_assign(a, parse_assign());
    if (c > 0 && ct_is_scalar(c) && ct_mty_agrees(c, ty)) {
        a->lhs->cty = ct_unqual(c);
        a->attr = AT_INIT;
    }
    local_init_append(nd_expr_stmt(a));
}

static Node *local_init_list_expr(Node *stmts, Node *result) {
    Node *s;
    Node *expr;

    if (stmts == NULL) return result;
    expr = stmts->lhs;
    s = stmts->next;
    while (s != NULL) {
        expr = nd_comma(expr, s->lhs);
        s = s->next;
    }
    return nd_comma(expr, result);
}

/* (char[]){"abc"}: a character array from its string literal
 * (6.7.9p14), the cursor on the string inside the literal's braces.
 * It went through the element list, and its one element was the
 * string's address: the array was empty (and the survey of 6.5.16.1
 * had it as a pointer stored into a char).  The hidden array gets the
 * string's bytes and zeros to its length; returns the stores, then the
 * array. */
static Node *ps_cl_string(char *nm, int ty, int count) {
    Node *head;
    Node *tail;
    Node *a;
    Node *n;
    char *sp;
    int sp_idx;
    int slen;
    int off;
    int ci;
    sp_idx = parse_string_literal();
    ps_braced_string_close();
    slen = lex_str_len[sp_idx];
    if (count < 0) count = slen + 1;
    off = add_local_array(nm, ty, count);
    sp = lex_strpool + lex_str_off[sp_idx];
    head = NULL;
    tail = NULL;
    ci = 0;
    while (ci < count) {
        n = nd_var(nm, off, ty + TY_PTR);
        ps_bind_var(n, ps_nlocals - 1);
        a = nd_unary(TK_STAR, nd_binop(TK_PLUS, n, nd_num(ci)));
        a = nd_expr_stmt(nd_assign(a, nd_num(ci < slen ? (sp[ci] & 255) : 0)));
        if (head == NULL) head = a;
        else tail->next = a;
        tail = a;
        ci = ci + 1;
    }
    n = nd_var(nm, off, ty + TY_PTR);
    ps_bind_var(n, ps_nlocals - 1);
    return local_init_list_expr(head, n);
}

/* A compound literal builds its hidden local through the same
 * local_init_* machinery a declarator uses, and that machinery keeps its
 * in-progress state in the ps_li_* globals.  A literal that appears
 * INSIDE another local's initializer -- `int *q = &(int){ 7 };`,
 * `int y = 3 + (int){ 7 };`, `struct pair *p = &(struct pair){ i, i*i }`
 * -- therefore clobbered the outer declarator's list and slot, and the
 * outer local was initialized from the wrong list or not at all
 * (GitHub issue 79: d25/d26 in the cross-compiler corpus, and the same
 * on SLOW-32; the assignment form `q = &(int){7}` was never affected).
 * Save the outer state on entry and put it back on every return. */
/* A compound literal at file scope has static storage (6.5.2.5p5): an
 * anonymous TU-local global, its braces read as a static initializer,
 * and the literal that global -- so &(struct P){11, 12} and
 * (int[]){4, 5, 6} are address constants.  It was built as a hidden
 * local, and there are none at file scope. */
static int ps_file_clit_id;
static Node *ps_file_compound_literal(int ty, int arr_count) {
    char nm[64];
    int gi;
    int cnt;
    int k;
    int v;
    int cols;
    int sub;
    Node *n;
    cols = ps_cl_cols;
    sub = ps_cl_sub;
    ps_cl_cols = 0;
    /* "__fclit." and the number */
    memcpy(nm, "__fclit.", 9);
    k = 8;
    v = ps_file_clit_id;
    ps_file_clit_id = ps_file_clit_id + 1;
    if (v >= 1000) { nm[k] = 48 + (v / 1000) % 10; k = k + 1; }
    if (v >= 100) { nm[k] = 48 + (v / 100) % 10; k = k + 1; }
    if (v >= 10) { nm[k] = 48 + (v / 10) % 10; k = k + 1; }
    nm[k] = 48 + v % 10;
    nm[k + 1] = 0;
    if (arr_count != 0) {
        gi = add_defined_global(nm, ty + TY_PTR, 0);
        ps_glocal[gi] = 1;
        ps_ginit_begin(gi);
        if (cols > 0) {
            ps_gcols[gi] = cols;
            ps_gsub[gi] = sub;
            cnt = cols * parse_global_init_array2d_at(ty, arr_count > 0 ? arr_count / cols : -1,
                                                      cols, sub, gi, 0);
        } else {
            cnt = parse_global_init_array_at(ty, arr_count, gi, 0);
        }
        ps_gsize[gi] = ty_size(ty) * cnt;
        ps_gcty[gi] = ps_array_cty(ct_from_mty(ty, 0), cnt, cols, sub);
        ps_ginit_ensure_len(gi, ps_gsize[gi]);
        ps_ginit_finish(gi);
    } else if (ty_is_struct(ty)) {
        gi = add_defined_global(nm, ty, 0);
        ps_glocal[gi] = 1;
        ps_ginit_begin(gi);
        parse_global_init_value(ty, gi);
        ps_ginit_finish(gi);
    } else {
        gi = add_defined_global(nm, ty, 0);
        ps_glocal[gi] = 1;
        expect(TK_LBRACE);
        ps_set_scalar_init(gi, parse_const_int(), 0);
        if (lex_tok == TK_COMMA) next();
        expect(TK_RBRACE);
    }
    n = nd_var(ps_gname[gi], 0, ps_gtype[gi]);
    n->cty = ps_gcty[gi];
    n->is_local = 0;
    n->is_array = arr_count != 0;
    n->arr_cols = ps_gcols[gi];
    n->arr_sub = ps_gsub[gi];
    return n;
}

static Node *parse_compound_literal_expr(int ty, int arr_count) {
    char nm[256];
    int off;
    Node *head;
    Node *zhead;
    Node *ztail;
    Node *result;
    Node *expr;
    int li;
    char *sv_name;
    int sv_off;
    int sv_slot;
    int sv_base_ty;
    int sv_base_is_array;
    Node *sv_head;
    Node *sv_tail;
    int cols;
    int sub;

    if (!ps_in_body) return ps_file_compound_literal(ty, arr_count);
    cols = ps_cl_cols;      /* before the initializer: it may hold another literal */
    sub = ps_cl_sub;
    ps_cl_cols = 0;
    sv_name = ps_li_name;
    sv_off = ps_li_off;
    sv_slot = ps_li_slot;
    sv_base_ty = ps_li_base_ty;
    sv_base_is_array = ps_li_base_is_array;
    sv_head = ps_li_head;
    sv_tail = ps_li_tail;

    ps_make_compound_literal_name(nm);
    if (arr_count != 0) ps_braced_string_open(ps_is_char_ty(ty));
    if (arr_count != 0 && ps_str_braced) {
        expr = ps_cl_string(nm, ty, arr_count);
    } else if (arr_count != 0) {
        local_init_begin(nm, 0, ty + TY_PTR, 1);
        if (cols > 0) {
            /* (T[N][M]){...}: its rows */
            expect(TK_LBRACE);
            arr_count = cols * ps_local_init_rows_in(ty, arr_count, ps_sub(cols, sub), 0);
        } else {
            arr_count = parse_local_init_array_at(ty, arr_count, 0);
        }
        head = ps_li_head;
        off = add_local_array(nm, ty, arr_count);
        if (cols > 0) {
            ps_lcols[ps_nlocals - 1] = cols;
            ps_lsub[ps_nlocals - 1] = sub;
            ps_local_rows_cty(ps_nlocals - 1);
        }
        local_init_patch_offsets(head, nm, off);

        local_init_begin(nm, off, ty + TY_PTR, 1);
        local_init_zero_at(ty + TY_PTR, arr_count, 0);
        zhead = ps_li_head;
        ztail = ps_li_tail;
        if (zhead != NULL) {
            ztail->next = head;
            head = zhead;
        }
        result = nd_var(nm, off, ty + TY_PTR);
        ps_bind_var(result, ps_nlocals - 1);
        expr = local_init_list_expr(head, result);
    } else {
        off = add_local(nm, ty);
        li = ps_nlocals - 1;   /* before the initializer: it may nest another literal */
        local_init_begin(nm, off, ty, 0);
        local_init_zero_at(ty, 0, 0);
        if (ty_is_struct(ty)) {
            parse_local_init_struct_at(ty, 0);
        } else {
            expect(TK_LBRACE);
            if (lex_tok != TK_RBRACE) {
                parse_local_init_value_at(ty, 0, 0);
                if (lex_tok == TK_COMMA) next();
            }
            expect(TK_RBRACE);
        }
        result = nd_var(nm, off, ty);
        ps_bind_var(result, li);
        expr = local_init_list_expr(ps_li_head, result);
    }
    /* A compound literal is an lvalue (6.5.2.5p4): its stores, then its
     * hidden variable -- a comma sema reads as the variable. */
    if (expr->kind == ND_COMMA) expr->op = TK_LBRACE;

    ps_li_name = sv_name;
    ps_li_off = sv_off;
    ps_li_slot = sv_slot;
    ps_li_base_ty = sv_base_ty;
    ps_li_base_is_array = sv_base_is_array;
    ps_li_head = sv_head;
    ps_li_tail = sv_tail;
    return expr;
}

/* --- Block-scope declarators --- */

/* The dimensions of a block-scope array declarator, cursor on `[`.
 * After the first, each must be sized; past two, the stride entry of
 * the rest is left in ps_dims_sub (ast.h ast_sub_cols) for the caller
 * to record beside the row width. */
static void parse_local_array_dims(int *pcount, int *plcols2) {
    int count;
    int lcols2;
    int inner;

    /* A declarator of a typedef'd array type brings that typedef's
     * dimension with it (ps_decl_inner, set where the declaration's
     * type was read): alone it is the array's, and under dimensions of
     * the declarator's own it is the innermost. */
    inner = ps_decl_inner;
    ps_decl_inner = 0;
    ps_dims_sub = 0;
    if (lex_tok != TK_LBRACK) {
        *pcount = inner;
        *plcols2 = 0;
        return;
    }
    next();
    count = -1;
    if (lex_tok != TK_RBRACK) {
        count = parse_const_int();
    }
    expect(TK_RBRACK);
    lcols2 = ps_inner_dims(inner, &ps_dims_sub);
    if (count >= 0 && lcols2 > 0) count = count * lcols2;
    *pcount = count;
    *plcols2 = lcols2;
}

/* Global gi takes the row width of an array declarator just read, and
 * its stride entry (ps_dims_sub). */
static void ps_set_gcols(int gi, int cols) {
    ps_gcols[gi] = cols;
    ps_gsub[gi] = cols > 0 ? ps_dims_sub : 0;
}

/* One non-static block-scope declarator after its name and stars:
 * the dimensions and the initializer, if any.  Allocates the local
 * and returns its initializing statements (NULL when there are none),
 * stopping at the `,` or `;` so parse_stmt can loop over a declarator
 * list.  Before this every shape returned from its own early exit,
 * so a `,` after a brace initializer, or a `[` after a `,`, was a
 * parse error (GitHub #8: `cob_num a = { getv(), 2 }, b = st[n];`,
 * `int a[2] = { 1, 2 }, b = 3;`, `int a = 1, b[2];`). */
static Node *ps_local_declarator_body(char *nm, int ty);

/* One declarator of a local declaration and its initializer: the body
 * below, and then a 2-D array gets its rows (ps_local_rows_cty), once
 * the initializer's own stores are written. */
static Node *parse_local_declarator(char *nm, int ty) {
    Node *t;
    int li;
    t = ps_local_declarator_body(nm, ty);
    li = find_local(nm);
    if (li >= 0) ps_local_rows_cty(li);
    return t;
}

static Node *ps_local_declarator_body(char *nm, int ty) {
    Node *n;
    Node *a;
    Node *head;
    Node *tail;
    Node *zhead;
    Node *ztail;
    int off;
    int count;
    int lcols2;
    int lsub;
    int ci;
    int cv;
    int sp_idx;
    int slen;
    char *sp;

    if (ps_vla_object_ahead()) return ps_local_vla(nm, ps_type_cty);
    if (lex_tok == TK_LBRACK || ps_decl_inner > 0) {
        parse_local_array_dims(&count, &lcols2);
        lsub = ps_dims_sub;     /* before an initializer can declare arrays of its own */
        head = NULL;
        if (lex_tok == TK_ASSIGN) {
            next();
            ps_braced_string_open(lcols2 == 0 && ps_is_char_ty(ty));
            if (lex_tok == TK_STRING) {
                /* String array init: char s[N] = "str" or char s[] = "str" */
                sp_idx = parse_string_literal();
                ps_braced_string_close();
                slen = lex_str_len[sp_idx];
                ps_init_string_fits(sp_idx, count);
                if (count < 0) count = slen + 1;
                off = add_local_array(nm, ty, count);
                sp = lex_strpool + lex_str_off[sp_idx];
                tail = NULL;
                ci = 0;
                while (ci < slen && ci < count) {
                    n = nd_var(nm, off, ty + TY_PTR);
                    ps_bind_var(n, ps_nlocals - 1);
                    a = nd_binop(TK_PLUS, n, nd_num(ci));
                    a = nd_unary(TK_STAR, a);
                    a = nd_assign(a, nd_num(sp[ci] & 255));
                    a = nd_expr_stmt(a);
                    if (head == NULL) { head = a; tail = a; }
                    else { tail->next = a; tail = a; }
                    ci = ci + 1;
                }
                if (ci < count) {
                    /* null terminator */
                    n = nd_var(nm, off, ty + TY_PTR);
                    ps_bind_var(n, ps_nlocals - 1);
                    a = nd_binop(TK_PLUS, n, nd_num(ci));
                    a = nd_unary(TK_STAR, a);
                    a = nd_assign(a, nd_num(0));
                    a = nd_expr_stmt(a);
                    if (head == NULL) { head = a; tail = a; }
                    else { tail->next = a; tail = a; }
                }
            } else if (lex_tok == TK_LBRACE && lcols2 > 0) {
                /* Local 2D char array with string rows:
                 * char name[23][8] = { "e2m1", ... } (d_main).  Each
                 * row gets its string's bytes, zero-padded to cols;
                 * a string of exactly cols chars drops its NUL (C
                 * semantics — doom's "spida1d1" needs this). */
                next();
                if ((ty & TY_BASE_MASK) != TY_CHAR || (ty & TY_PTR_MASK) != 0 ||
                    lex_tok != TK_STRING) {
                    /* Numeric rows: T a[N][M] = { {..}, {..} } (regal's
                     * valuation.c: double augmented[3][4] with runtime
                     * values).  Each row is an M-element array
                     * initializer at its own byte offset; the same
                     * parse-then-patch shape as the 1D brace path
                     * below, zero-filled first.  A row may elide its
                     * braces (bounded by M). */
                    local_init_begin(nm, 0, ty + TY_PTR, 1);
                    cv = ps_local_init_rows_in(ty, count, ps_sub(lcols2, lsub), 0);   /* rows seen */
                    if (count < 0) count = cv * lcols2;
                    head = ps_li_head;
                    tail = ps_li_tail;
                    off = add_local_array(nm, ty, count);
                    ps_lcols[ps_nlocals - 1] = lcols2;
                ps_lsub[ps_nlocals - 1] = lsub;
                    local_init_patch_offsets(head, nm, off);
                    local_init_begin(nm, off, ty + TY_PTR, 1);
                    local_init_zero_at(ty + TY_PTR, count, 0);
                    zhead = ps_li_head;
                    ztail = ps_li_tail;
                    if (zhead != NULL) {
                        if (ztail == NULL) {
                            ztail = zhead;
                            while (ztail->next != NULL) ztail = ztail->next;
                        }
                        ztail->next = head;
                        head = zhead;
                        if (tail == NULL) tail = ztail;
                    }
                    return head;
                }
                if (count < 0) {
                    p_error("array size required for char[][M] string rows");
                    return nd_num(0);
                }
                off = add_local_array(nm, ty, count);
                ps_lcols[ps_nlocals - 1] = lcols2;
                ps_lsub[ps_nlocals - 1] = lsub;
                head = NULL;
                tail = NULL;
                ci = 0;
                while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                    if (ci > 0) {
                        expect(TK_COMMA);
                        if (lex_tok == TK_RBRACE) break;
                    }
                    if (lex_tok != TK_STRING) {
                        p_error("expected string row in 2D char array init");
                        return nd_num(0);
                    }
                    sp_idx = parse_string_literal();
                    sp = lex_strpool + lex_str_off[sp_idx];
                    slen = lex_str_len[sp_idx];
                    cv = 0;
                    while (cv < lcols2) {
                        n = nd_var(nm, off, ty + TY_PTR);
                        ps_bind_var(n, ps_nlocals - 1);
                        a = nd_binop(TK_PLUS, n, nd_num(ci * lcols2 + cv));
                        a = nd_unary(TK_STAR, a);
                        a = nd_assign(a, nd_num((cv < slen) ? (sp[cv] & 255) : 0));
                        a = nd_expr_stmt(a);
                        if (head == NULL) { head = a; tail = a; }
                        else { tail->next = a; tail = a; }
                        cv = cv + 1;
                    }
                    ci = ci + 1;
                }
                if (lex_tok == TK_COMMA) next();
                expect(TK_RBRACE);
                return head;
            } else if (lex_tok == TK_LBRACE) {
                local_init_begin(nm, 0, ty + TY_PTR, 1);
                count = parse_local_init_array_at(ty, count, 0);
                head = ps_li_head;
                tail = ps_li_tail;
                off = add_local_array(nm, ty, count);
                local_init_patch_offsets(head, nm, off);
                local_init_begin(nm, off, ty + TY_PTR, 1);
                local_init_zero_at(ty + TY_PTR, count, 0);
                zhead = ps_li_head;
                ztail = ps_li_tail;
                if (zhead != NULL) {
                    if (ztail == NULL) {
                        ztail = zhead;
                        while (ztail->next != NULL) ztail = ztail->next;
                    }
                    ztail->next = head;
                    head = zhead;
                    if (tail == NULL) tail = ztail;
                }
            } else {
                p_error("expected string or { in array init");
                return nd_num(0);
            }
        } else {
            if (count < 0) {
                p_error("array size required without initializer");
                return nd_num(0);
            }
            off = add_local_array(nm, ty, count);
            ps_lcols[ps_nlocals - 1] = lcols2;
            ps_lsub[ps_nlocals - 1] = lsub;
        }
        return head;
    }

    /* Scalar or struct: type name; or type name = expr; */
    off = add_local(nm, ty);
    if (lex_tok == TK_ASSIGN) {
        /* Bind to THIS local, captured before the initializer is parsed:
         * a compound literal inside it adds a hidden local of its own,
         * and "the most recent local" is then the literal's, not ours
         * (GitHub issue 79 -- `int *q = &(int){ 7 };` bound q to the
         * literal's slot and read garbage). */
        int li;
        li = ps_nlocals - 1;
        next();
        if (lex_tok == TK_LBRACE && ty_is_struct(ty)) {
            local_init_begin(nm, off, ty, 0);
            local_init_zero_at(ty, 0, 0);
            parse_local_init_struct_at(ty, 0);
            return ps_li_head;
        }
        if (lex_tok == TK_LBRACE) {
            /* C89 lets a scalar initializer wear braces: int k = { 1 }; */
            next();
            n = nd_assign(nd_var(nm, off, ty), parse_assign());
            if (lex_tok == TK_COMMA) next();
            expect(TK_RBRACE);
        } else {
            n = nd_assign(nd_var(nm, off, ty), parse_assign());
        }
        ps_bind_var(n->lhs, li);
        return nd_expr_stmt(n);
    }
    return NULL;
}


/* --- Statement parser --- */

static Node *parse_block(void);

/* Shared typedef declaration body (after the `typedef` keyword).
 * Called from both file scope and statement scope — vecscope declares
 * `typedef struct {...} key_t;` inside main.  Names register in the
 * file-scope typedef table either way (fine for this C subset). */
/* The typedef just added, against an earlier one of the name in the
 * same scope: a redefinition must name the same type (6.7p3).  The
 * compiler's own (size_t, bool) are left to the headers. */
static void ps_typedef_check(void) {
    int n;
    int i;
    n = ps_ntypedefs - 1;
    i = n - 1;
    while (i >= 0 && strcmp(ps_tdname[i], ps_tdname[n]) != 0) i = i - 1;
    if (i < 0 || ps_tddepth[i] != ps_tddepth[n]) return;
    if (ps_tdcty[i] > 0 && ps_tdcty[n] > 0 && !sema_compat(ps_tdcty[i], ps_tdcty[n]))
        p_error("typedef redefinition with different types");
}

/* The compiler's own typedefs, installed before the source: exempt. */
static void ps_typedefs_builtin(void) {
    int i;
    i = 0;
    while (i < ps_ntypedefs) {
        ps_tddepth[i] = -1;
        i = i + 1;
    }
}

static void parse_typedef_decl(void) {
    int ty;
    int cty;
    int bty;
    int bcty;
    char nm[256];
        ty = parse_type();
        if (ps_type_alignas) p_error("_Alignas may not appear in a typedef");   /* 6.7.5p2 */
        cty = ps_type_cty;     /* before a parameter list runs parse_type again */
        bty = ps_type_nostar;  /* the base, for the declarators after a ',' */
        bcty = ps_type_nostar_cty;
        if (lex_tok == TK_LPAREN) {
            /* Function pointer typedef: typedef int (*Name)(args); */
            next();  /* skip ( */
            ps_skip_fnptr_stars();   /* (**name)(...) */
            if (lex_tok != TK_IDENT) {
                p_error("expected typedef name");
                return;
            }
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            expect(TK_RPAREN);
            /* Record the parameter list so a call through this typedef
             * converts i64 args (GitHub issue 41). */
            {
                int tfpb;
                int tfpn;
                tfpb = -1;
                tfpn = 0;
                if (lex_tok == TK_LPAREN) {
                    next();
                    tfpb = ps_parse_fp_params(&tfpn);
                    ps_fp_set_ret(tfpb, ty);
                }
                add_typedef(nm, TY_INT);
                /* a pointer to a function (no prototype recorded yet: S2) */
                ps_tdcty[ps_ntypedefs - 1] = ps_fnptr_cty(cty);
                ps_tdfpbase[ps_ntypedefs - 1] = tfpb;
                ps_tdfpn[ps_ntypedefs - 1] = tfpn;
                ps_typedef_check();
            }
            expect(TK_SEMI);
            return;
        }
        if (lex_tok != TK_IDENT) {
            p_error("expected typedef name");
            return;
        }
        memcpy(nm, lex_str, lex_slen + 1);
        next();
        if (lex_tok == TK_LPAREN) {
            next();
            while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next();
            expect(TK_RPAREN);
            add_typedef(nm, TY_INT);  /* function types decay to pointer use-sites here */
            ps_tdcty[ps_ntypedefs - 1] = ct_func(cty, 0, -1, 0);
            ps_typedef_check();
            skip_gnu_decl_suffixes();
            expect(TK_SEMI);
            return;
        }
        if (lex_tok == TK_LBRACK) {
            int tdcount;
            if (ps_vla_ahead()) {
                ps_vla_typedef(nm, cty);
                return;
            }
            next();
            tdcount = 0;
            if (lex_tok != TK_RBRACK) tdcount = parse_const_int();
            expect(TK_RBRACK);
            add_typedef(nm, ty + TY_PTR);
            ps_tdcty[ps_ntypedefs - 1] = ct_array(cty, tdcount > 0 ? tdcount : -1);
            /* Object declarations of this type must materialize the
             * array; parameters still decay. */
            ps_tdarr[ps_ntypedefs - 1] = tdcount;
            ps_typedef_check();
            skip_gnu_decl_suffixes();
            expect(TK_SEMI);
            return;
        }
        add_typedef(nm, ty);
        ps_tdcty[ps_ntypedefs - 1] = cty;
        ps_typedef_check();
        skip_gnu_decl_suffixes();
        /* typedef int A, *AP; -- each declarator its own name (6.7p1) */
        while (lex_tok == TK_COMMA) {
            next();
            ty = ps_declarator_stars(bty, bcty);
            cty = ps_type_cty;
            if (lex_tok != TK_IDENT) {
                p_error("expected typedef name");
                return;
            }
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            add_typedef(nm, ty);
            ps_tdcty[ps_ntypedefs - 1] = cty;
            ps_typedef_check();
            skip_gnu_decl_suffixes();
        }
        expect(TK_SEMI);
        return;
    }

/* A for statement's first clause when it is a declaration (6.8.5p3):
 * read as one, by parse_stmt -- several declarators, arrays and their
 * initializers, struct types, register.  It took one name, `*`s and an
 * optional `= expr`.  A single `name = value` is still the loop's init
 * expression, as before; anything else comes back in *pre, statements
 * to run ahead of the loop, in its scope. */
static Node *ps_for_decl(Node **pre) {
    Node *d;
    d = parse_stmt();
    if (d && d->kind == ND_EXPR_STMT && d->lhs && d->lhs->kind == ND_ASSIGN && d->next == NULL)
        return d->lhs;
    if (d && d->kind == ND_BLOCK && d->body == NULL) return NULL;
    *pre = d;
    return NULL;
}

/* The for statement, its declaration's statements (if any) first. */
static Node *ps_for_node(Node *pre, Node *f) {
    if (pre == NULL) return f;
    pre->next = f;
    return nd_block(pre);
}

/* A function declared at block scope, `void WI_unloadData(void);`,
 * the cursor on its '(': registered as a file-scope prototype is
 * (6.2.2p5: it has external linkage).  It was skipped, and the call
 * after it taken as an implicit declaration -- which went with C99. */
static Node *ps_local_prototype(char *nm, int ty) {
    int ptys[64];
    int pctys[64];
    int base;
    int np;
    int i;
    int ret_cty;
    int sv_var;
    ret_cty = ps_type_cty;
    next();
    base = ps_parse_fp_params(&np);
    i = 0;
    while (i < np && i < 64 && base >= 0) {
        ptys[i] = ps_fptypes[base + i];
        pctys[i] = ps_fpctypes[base + i];
        i = i + 1;
    }
    sv_var = ps_is_varargs;
    ps_is_varargs = base >= 0 ? ps_fpvar[base] : 0;
    add_func_type(nm, ty, ptys, pctys, base >= 0 ? np : -1, ret_cty);
    ps_is_varargs = sv_var;
    skip_gnu_decl_suffixes();
    expect(TK_SEMI);
    return nd_block(NULL);
}

/* An array's size, read: positive (6.7.6.2p1) -- zero is GNU's, and
 * taken.  (-1 is what the callers use for no size at all.) */
static int ps_array_size(void) {
    int n;
    n = parse_const_int();
    if (n < 0) p_error("array has a negative size");
    return n;
}

/* An array declared with no size and no initializer: an extern
 * reference, or a tentative definition, one element unless a later
 * declaration completes it (6.9.2p2) -- but not one with internal
 * linkage, which must be complete (6.9.2p3). */
static int ps_tentative_count(int is_static) {
    if (is_static) p_error("tentative definition with internal linkage has incomplete type");
    ps_decl_unsized = 1;
    return 1;
}

static Node *parse_stmt(void) {
    int tdac;
    int td_elem;
    Node *n;
    Node *c;
    Node *t;
    Node *e;
    Node *head;
    Node *tail;
    Node *a;
    Node *zhead;
    Node *ztail;
    int ty;
    int off;
    int count;
    int count2;
    int lcols2;
    int ci;
    int ci_stack;
    int cv;
    int cv_hi;
    int neg;
    int si;
    int base;
    int base_cty;
    int nf;
    int nsi;
    int nnf;
    int nj;
    int mi;
    int mi_off;
    int mi_ty;
    int xty;
    int sp_idx;
    int slen;
    int saw_unnamed_param;
    char *sp;
    char nm[256];
    char dnm[256];

    /* GNU inline asm statement. */
    if (is_gnu_asm_ident()) {
        n = parse_gnu_asm_stmt();
        expect(TK_SEMI);
        return nd_expr_stmt(n);
    }

    /* Function-scope _Static_assert(expr, "message") */
    if (lex_tok == TK_STATIC_ASSERT) {
        int sa_val;
        Node *sa_expr;
        next();
        expect(TK_LPAREN);
        sa_expr = parse_assign();
        sa_val = ps_static_assert_value(sa_expr);
        expect(TK_COMMA);
        while (lex_tok == TK_STRING) next(); /* message may be adjacent literals */
        expect(TK_RPAREN);
        expect(TK_SEMI);
        if (sa_val == 0) p_error("_Static_assert failed");
        return nd_block(NULL);
    }

    /* null statement */
    if (lex_tok == TK_SEMI) {
        next();
        return nd_block(NULL);
    }

    /* return statement */
    if (lex_tok == TK_RETURN) {
        next();
        if (lex_tok == TK_SEMI) {
            next();
            return nd_return(NULL);
        }
        n = parse_expr();
        expect(TK_SEMI);
        return nd_return(n);
    }

    /* if statement */
    if (lex_tok == TK_IF) {
        next();
        expect(TK_LPAREN);
        c = parse_expr();
        expect(TK_RPAREN);
        t = parse_stmt();
        e = NULL;
        if (lex_tok == TK_ELSE) {
            next();
            e = parse_stmt();
        }
        return nd_if(c, t, e);
    }

    /* do/while statement */
    if (lex_tok == TK_DO) {
        next();
        t = parse_stmt();
        if (lex_tok != TK_WHILE) {
            p_error("expected 'while' after do body");
        }
        next();
        expect(TK_LPAREN);
        c = parse_expr();
        expect(TK_RPAREN);
        expect(TK_SEMI);
        return ps_vla_loop(nd_do_while(c, t), 1);
    }

    /* while statement */
    if (lex_tok == TK_WHILE) {
        next();
        expect(TK_LPAREN);
        c = parse_expr();
        expect(TK_RPAREN);
        t = parse_stmt();
        return ps_vla_loop(nd_while(c, t), 1);
    }

    /* for statement (first-class node) */
    if (lex_tok == TK_FOR) {
        next();
        expect(TK_LPAREN);
        ci = ps_scope_begin();  /* save scope for for-init declarations */
        ci_stack = ps_stack;
        a = NULL;
        /* init */
        if (lex_tok == TK_SEMI) {
            next();
            n = NULL;
        } else if (is_type() || lex_tok == TK_REGISTER) {
            n = ps_for_decl(&a);     /* for (int i = 0; ...), and any declaration */
        } else {
            n = parse_expr();
            expect(TK_SEMI);
        }
        /* cond */
        if (lex_tok == TK_SEMI) {
            c = nd_num(1);
            next();
        } else {
            c = parse_expr();
            expect(TK_SEMI);
        }
        /* step */
        if (lex_tok == TK_RPAREN) {
            e = NULL;
            next();
        } else {
            e = parse_expr();
            expect(TK_RPAREN);
        }
        /* body */
        t = parse_stmt();
        ps_scope_end(ci);
        if (!ps_fn_setjmp) ps_stack = ci_stack;
        return ps_for_node(a, ps_vla_loop(nd_for(n, c, e, t), 1));
    }

    /* switch statement */
    if (lex_tok == TK_SWITCH) {
        next();
        expect(TK_LPAREN);
        c = parse_expr();
        expect(TK_RPAREN);
        t = parse_stmt();
        /* An unbraced body that is a label -- switch (x) case 9: r = 99;
         * or switch (x) default: ... -- is the label and the statement it
         * labels.  A label parses as a statement of its own, so the
         * labelled statement became the one after the switch, run
         * always, and the switch had no case blocks (6.8.4.2). */
        if (t && (t->kind == ND_CASE || t->kind == ND_DEFAULT)) {
            Node *last;
            last = t;
            while (last && (last->kind == ND_CASE || last->kind == ND_DEFAULT)) {
                last->next = parse_stmt();
                last = last->next;
            }
            t = nd_block(t);
        }
        n = nd_new(ND_SWITCH);
        n->cond = c;
        n->body = t;
        return ps_vla_loop(n, 0);
    }

    /* case label: any constant expression (doom packs chars with
     * shifts: case AM_MSGENTERED == (('a'<<24)+...)+('e'<<8)) */
    if (lex_tok == TK_CASE) {
        next();
        neg = 0;
        cv_hi = ps_case_value(&cv);   /* both words: a long long switch compares them */
        if (neg) cv = 0 - cv;
        expect(TK_COLON);
        n = nd_new(ND_CASE);
        n->val = cv;
        n->val_hi = cv_hi;
        return ps_vla_jump(n);
    }

    /* default label */
    if (lex_tok == TK_DEFAULT) {
        next();
        expect(TK_COLON);
        return ps_vla_jump(nd_new(ND_DEFAULT));
    }

    /* break statement */
    if (lex_tok == TK_BREAK) {
        next();
        expect(TK_SEMI);
        return ps_vla_jump(nd_new(ND_BREAK));
    }

    /* continue statement */
    if (lex_tok == TK_CONTINUE) {
        next();
        expect(TK_SEMI);
        return ps_vla_jump(nd_new(ND_CONTINUE));
    }

    /* goto statement */
    if (lex_tok == TK_GOTO) {
        next();
        if (lex_tok != TK_IDENT) {
            p_error("expected label name after goto");
        }
        memcpy(nm, lex_str, lex_slen + 1);
        next();
        expect(TK_SEMI);
        return ps_vla_jump(nd_goto(find_or_add_label(nm)));
    }

    /* block */
    if (lex_tok == TK_LBRACE) {
        return parse_block();
    }

    /* Track static qualifier before local declarations */
    {
    int is_static;
    int is_extern;
    int sl_gi;
    int sl_li;
    is_static = 0;
    is_extern = 0;
    skip_gnu_attributes();
    /* Function-scope typedef (vecscope: typedef struct {...} key_t;) */
    if (lex_tok == TK_TYPEDEF) {
        next();
        parse_typedef_decl();
        return nd_block(ps_vla_pend_stmts(NULL));
    }
    while (lex_tok == TK_STATIC || lex_tok == TK_CONST ||
           lex_tok == TK_EXTERN ||
           lex_tok == TK_REGISTER || lex_tok == TK_RESTRICT ||
           lex_tok == TK_AUTO || is_gnu_qual_ident() ||
           is_gnu_extension_ident() || is_gnu_inline_ident() ||
           is_gnu_attr_ident()) {
        if (lex_tok == TK_STATIC) is_static = 1;
        if (lex_tok == TK_EXTERN) is_extern = 1;
        if (is_gnu_attr_ident()) skip_gnu_attributes();
        else ps_storage_next();
    }

    /* local variable declaration */
    if (is_type()) {
        int was_enum;
        was_enum = (lex_tok == TK_ENUM);
        ty = parse_type();
        base = ps_type_nostar;   /* before an initializer parses a type */
        base_cty = ps_type_nostar_cty;
        tdac = ps_type_arrcount;
        td_elem = -1;
        skip_decl_qualifiers();
        /* Bare tag definition as a statement: `enum { A, B };` or
           `struct S { ... };` -- no declarator follows. */
        if (lex_tok == TK_SEMI && (ty_is_struct(ty) || was_enum)) {
            next();
            return nd_block(NULL);
        }
        /* Function pointer declaration: type (*name)(args); */
        if (lex_tok == TK_LPAREN) {
            int nstars;
            int has_params;
            next();
            if (ps_try_grouping_paren()) goto local_plain_name;
            nstars = 0;
            while (lex_tok == TK_STAR) {     /* (**name)(...) */
                nstars = nstars + 1;
                next();
            }
            if (lex_tok != TK_IDENT) {
                p_error("expected identifier in fn ptr decl");
                return nd_num(0);
            }
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            /* Array of function pointers: type (*name[N])(args)
             * (f_wipe's static wipes[] table).  Elements are word-
             * sized; a static one lives in .data with symbol-reloc
             * initializers.  Without (params), T (*name[N]) is T *name[N]
             * (GitHub issue 47). */
            if (lex_tok == TK_LBRACK) {
                int fpcount;
                int fpi;
                int afpb;
                int afpn;
                char fpn[256];
                next();
                fpcount = -1;
                afpb = -1;
                afpn = 0;
                if (lex_tok != TK_RBRACK) fpcount = parse_const_int();
                expect(TK_RBRACK);
                expect(TK_RPAREN);
                has_params = 0;
                if (lex_tok == TK_LPAREN) {
                    has_params = 1;
                    next();
                    afpb = ps_parse_fp_params(&afpn);
                    ps_fp_set_ret(afpb, ty);
                }
                if (!has_params) {
                    while (nstars > 0) {
                        ty = ty + TY_PTR;
                        nstars = nstars - 1;
                    }
                    if (is_static) {
                        ps_mangle_static(ps_cur_func, nm);
                        sl_gi = add_global(ps_sl_buf, ty + TY_PTR,
                                           (fpcount >= 0) ? ty_size(ty) * fpcount : 0);
                        ps_glocal[sl_gi] = 1;
                        if (lex_tok == TK_ASSIGN) {
                            next();
                            ps_ginit_begin(sl_gi);
                            fpcount = parse_global_init_array_at(ty, fpcount, sl_gi, 0);
                            ps_gsize[sl_gi] = ty_size(ty) * fpcount;
                            ps_ginit_ensure_len(sl_gi, ps_gsize[sl_gi]);
                            ps_ginit_finish(sl_gi);
                        } else if (fpcount < 0) {
                            p_error("array size required without initializer");
                            return nd_num(0);
                        }
                        expect(TK_SEMI);
                        if (ps_add_static_array_alias(nm, ty, 0, sl_gi) < 0) return nd_num(0);
                        return nd_block(NULL);
                    }
                    if (lex_tok == TK_ASSIGN) {
                        next();
                        local_init_begin(nm, 0, ty + TY_PTR, 1);
                        count = parse_local_init_array_at(ty, fpcount, 0);
                        head = ps_li_head;
                        off = add_local_array(nm, ty, count);
                        local_init_patch_offsets(head, nm, off);
                        expect(TK_SEMI);
                        return nd_block(head);
                    }
                    if (fpcount < 0) {
                        p_error("array size required without initializer");
                        return nd_num(0);
                    }
                    add_local_array(nm, ty, fpcount);
                    expect(TK_SEMI);
                    return nd_block(NULL);
                }
                if (!is_static) {
                    p_error("non-static local fn-ptr arrays unsupported");
                    return nd_num(0);
                }
                ps_mangle_static(ps_cur_func, nm);
                sl_gi = add_global(ps_sl_buf, TY_INT + TY_PTR,
                                   (fpcount >= 0) ? 4 * fpcount : 0);
                ps_glocal[sl_gi] = 1;
                if (lex_tok == TK_ASSIGN) {
                    next();
                    expect(TK_LBRACE);
                    ps_ginit_begin(sl_gi);
                    fpi = 0;
                    while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                        if (fpi > 0) {
                            expect(TK_COMMA);
                            if (lex_tok == TK_RBRACE) break;
                        }
                        if (lex_tok != TK_IDENT) {
                            p_error("expected function name in fn-ptr array init");
                            return nd_num(0);
                        }
                        memcpy(fpn, lex_str, lex_slen + 1);
                        next();
                        ps_ginit_add_sym_reloc_at(sl_gi, fpi * 4, fpn, 4);
                        fpi = fpi + 1;
                    }
                    if (lex_tok == TK_COMMA) next();
                    expect(TK_RBRACE);
                    if (fpcount < 0) fpcount = fpi;
                    ps_gsize[sl_gi] = 4 * fpcount;
                    ps_ginit_ensure_len(sl_gi, ps_gsize[sl_gi]);
                    ps_ginit_finish(sl_gi);
                } else if (fpcount < 0) {
                    p_error("fn-ptr array size required without initializer");
                    return nd_num(0);
                }
                expect(TK_SEMI);
                sl_li = ps_add_static_fnptr_alias(nm, nstars, sl_gi);
                if (sl_li < 0) return nd_num(0);
                ps_lfpbase[sl_li] = afpb;
                ps_lfpn[sl_li] = afpn;
                ps_gfpbase[sl_gi] = afpb;
                ps_gfpn[sl_gi] = afpn;
                return nd_block(NULL);
            }
            expect(TK_RPAREN);
            if (lex_tok == TK_LBRACK) return ps_local_rowptr(nm, ty, nstars, is_static);
            /* Record the parameter list so a call through this local
             * converts i64 args (GitHub issue 41). */
            {
                int lfpb;
                int lfpn;
                int lrcty;
                lfpb = -1;
                lfpn = 0;
                /* the return type's, before the parameters overwrite it:
                 * int (*fp)(void) was a pointer to a function returning void */
                lrcty = ps_type_cty;
                if (lex_tok == TK_LPAREN) {
                    next();
                    lfpb = ps_parse_fp_params(&lfpn);
                    ps_fp_set_ret(lfpb, ty);
                }
                ps_fnptr_nstar = nstars;
                ps_type_cty = ps_fnptr_cty(lrcty);
                off = add_local(nm, TY_INT);
                /* the machine type is int whatever the stars; the C type
                 * is what was declared -- void (**pp)(void) fell back to int */
                ps_lcty[ps_nlocals - 1] = ps_type_cty;
                ps_lfpbase[ps_nlocals - 1] = lfpb;
                ps_lfpn[ps_nlocals - 1] = lfpn;
            }
            if (lex_tok == TK_ASSIGN) {
                int li;
                li = ps_nlocals - 1;   /* before the initializer (issue 79) */
                next();
                n = nd_assign(nd_var(nm, off, TY_INT), parse_assign());
                ps_bind_var(n->lhs, li);
                expect(TK_SEMI);
                return nd_expr_stmt(n);
            }
            expect(TK_SEMI);
            return nd_block(NULL);
        }
local_plain_name:
        if (lex_tok != TK_IDENT) {
            p_error("expected identifier in declaration");
            return nd_num(0);
        }
        memcpy(nm, lex_str, lex_slen + 1);
        next();
        skip_gnu_decl_suffixes();

        /* Typedef'd array object: typedef byte sha1_digest_t[20];
         * sha1_digest_t digest;  — materialize the array. */
        if (tdac > 0 && lex_tok == TK_SEMI && !is_extern && !is_static) {
            off = add_local_array(nm, ty_deref(ty), tdac);
            next();
            return nd_block(NULL);
        }

        /* Any other declarator of a typedef'd array type: an array of
         * it (jmp_buf handlers[4]), a static one (static jmp_buf env;),
         * one with an initializer.  The element type is the typedef's,
         * and its dimension goes with the declarator
         * (parse_local_array_dims).  These were taken for pointers --
         * the typedef's type is spelled elem + TY_PTR -- so an array of
         * four jmp_bufs was four words, and a static one was one. */
        ps_decl_inner = 0;
        if (tdac > 0 && is_extern && lex_tok == TK_SEMI) {
            add_extern_global(nm, ty, ty_size(ty_deref(ty)) * tdac);
            next();
            return nd_block(NULL);
        }
        if (tdac > 0 && !is_extern &&
            (lex_tok == TK_LBRACK || lex_tok == TK_SEMI || lex_tok == TK_ASSIGN || lex_tok == TK_COMMA)) {
            ty = ty_deref(ty);
            ps_decl_inner = tdac;
            td_elem = ty;
        }

        /* Statement-scope function PROTOTYPE (doom's wi_stuff declares
         * `void WI_unloadData(void);` inside a function) */
        if (lex_tok == TK_LPAREN) return ps_local_prototype(nm, ty);

        /* Function-scope extern declaration: bind the name as global, but
         * do not allocate a stack local. */
        if (is_extern) {
            if (lex_tok == TK_LBRACK) {
                next();
                if (lex_tok != TK_RBRACK) parse_const_int();
                expect(TK_RBRACK);
                skip_gnu_decl_suffixes();
                add_extern_global(nm, ty + TY_PTR, 1);
            } else {
                add_extern_global(nm, ty, 0);
                /* With a local of the name visible, the extern names the
                 * file-scope object, not the local (6.2.2p4): an alias
                 * entry shadows it, as a static local's does, resolving
                 * to the global's own name.  Lookup found the local. */
                if (find_local(nm) >= 0 && ps_add_local_alias(nm, ty, nm) < 0)
                    return nd_num(0);
            }
            expect(TK_SEMI);
            return nd_block(NULL);
        }

        /* Static local scalar: emit as global with mangled name.  A
         * declarator list goes back and forth between this and the array
         * case below as each declarator's shape says: static char
         * cells[3], *pc = cells; */
static_scalar_decl:
        if (is_static && lex_tok != TK_LBRACK && ps_decl_inner == 0) {
            ps_mangle_static(ps_cur_func, nm);
            sl_gi = add_global(ps_sl_buf, ty,
                               ty_is_struct(ty) ? ty_size(ty) : 0);
            ps_glocal[sl_gi] = 1;
            if (lex_tok == TK_ASSIGN) {
                next();
                if (ty_is_struct(ty)) {
                    /* static event_t st_notify = { ev_keyup, ... }
                     * (am_map) — full struct initializer machinery. */
                    ps_ginit_begin(sl_gi);
                    parse_global_init_struct_at(ty, sl_gi, 0);
                    ps_ginit_finish(sl_gi);
                } else if (ty_is_llong(ty)) {
                    int glo;
                    int ghi;
                    ghi = parse_const_ll_hi(&glo);
                    ps_set_scalar_init(sl_gi, glo, ghi);
                } else if (lex_tok == TK_STRING) {
                    /* static const char *zFormat1 = "..." (SQLite's
                     * sqlite3VdbePrintOp): a string, like a file-scope pointer's. */
                    ps_set_str_init(sl_gi, parse_string_literal());
                } else if (ty_is_fp(ty)) {
                    ps_ginit_begin(sl_gi);
                    ps_fp_init_store_at(ty, sl_gi, 0);
                    ps_ginit_ensure_len(sl_gi, ty_size(ty));
                    ps_ginit_finish(sl_gi);
                } else if (ty_is_ptr(ty) &&
                           (lex_tok == TK_AMP ||
                            (lex_tok == TK_IDENT && find_const(lex_str) < 0 &&
                             (find_global(lex_str) >= 0 || ps_static_local_sym(lex_str))))) {
                    ps_ptr_init_at(sl_gi, ty);
                } else {
                    ps_set_scalar_init(sl_gi, parse_const_int(), 0);
                }
            }
            /* Register, then loop on comma for further declarators:
             * static int lastlevel = -1, lastepisode = -1; (am_map).
             * Each brings its own stars and qualifiers, as an automatic
             * local's do: static const char *const a = "x", *const b. */
            while (1) {
                sl_li = ps_add_local_alias(nm, ty, ps_sl_buf);
                if (sl_li < 0) return nd_num(0);
                if (lex_tok != TK_COMMA) break;
                next();
                ty = ps_declarator_stars(base, base_cty);
                skip_decl_qualifiers();
                if (lex_tok != TK_IDENT) { p_error("expected name after comma"); return nd_num(0); }
                memcpy(nm, lex_str, lex_slen + 1);
                next();
                if (lex_tok == TK_LBRACK) goto static_array_decl;
                ps_mangle_static(ps_cur_func, nm);
                sl_gi = add_global(ps_sl_buf, ty,
                                   ty_is_struct(ty) ? ty_size(ty) : 0);
                ps_glocal[sl_gi] = 1;
                if (lex_tok == TK_ASSIGN) {
                    next();
                    if (ty_is_llong(ty)) {
                        int glo;
                        int ghi;
                        ghi = parse_const_ll_hi(&glo);
                        ps_set_scalar_init(sl_gi, glo, ghi);
                    } else if (lex_tok == TK_STRING) {
                        /* static const char *zFormat1 = "..." (SQLite's
                         * sqlite3VdbePrintOp): a string, like a file-scope pointer's. */
                        ps_set_str_init(sl_gi, parse_string_literal());
                    } else if (ty_is_fp(ty)) {
                        ps_ginit_begin(sl_gi);
                        ps_fp_init_store_at(ty, sl_gi, 0);
                        ps_ginit_ensure_len(sl_gi, ty_size(ty));
                        ps_ginit_finish(sl_gi);
                    } else if (ty_is_ptr(ty) &&
                               (lex_tok == TK_AMP ||
                                (lex_tok == TK_IDENT && find_const(lex_str) < 0 &&
                                 (find_global(lex_str) >= 0 || ps_static_local_sym(lex_str))))) {
                        ps_ptr_init_at(sl_gi, ty);
                    } else {
                        ps_set_scalar_init(sl_gi, parse_const_int(), 0);
                    }
                }
            }
            expect(TK_SEMI);
            return nd_block(NULL);
        }

        /* Static local array: emit as global with mangled name */
static_array_decl:
        if ((lex_tok == TK_LBRACK || ps_decl_inner > 0) && is_static) {
          /* A static array declarator list: `static char a[4096],
           * b[4096];` (regal's test_keyword.c).  Each name gets its own
           * mangled global; the list stays arrays. */
          while (1) {
            parse_local_array_dims(&count, &lcols2);
            sp_idx = -1;
            neg = 0;
            if (count < 0) {
                if (lex_tok == TK_ASSIGN) {
                    next();
                    ps_braced_string_open(lcols2 == 0 && ps_is_char_ty(ty));
                    if (lex_tok == TK_STRING) {
                        sp_idx = parse_string_literal();
                        ps_braced_string_close();
                        count = lex_str_len[sp_idx] + 1;
                    } else if (lex_tok == TK_LBRACE) {
                        neg = 1;
                    } else {
                        p_error("array size required without initializer");
                        return nd_num(0);
                    }
                } else {
                    p_error("array size required without initializer");
                    return nd_num(0);
                }
            }
            if (sp_idx < 0 && lex_tok == TK_ASSIGN) {
                next();
                ps_braced_string_open(lcols2 == 0 && ps_is_char_ty(ty));
                if (lex_tok == TK_STRING) {
                    sp_idx = parse_string_literal();
                    ps_braced_string_close();
                    ps_init_string_fits(sp_idx, count);
                } else if (lex_tok == TK_LBRACE) {
                    neg = 1;
                } else {
                    p_error("static local array initializer unsupported");
                    return nd_num(0);
                }
            }
            ps_mangle_static(ps_cur_func, nm);
            sl_gi = add_global(ps_sl_buf, ty + TY_PTR,
                               (count >= 0) ? ty_size(ty) * count : 0);
            ps_glocal[sl_gi] = 1;
            ps_set_gcols(sl_gi, lcols2);
            if (sp_idx >= 0) {
                sp = lex_strpool + lex_str_off[sp_idx];
                slen = lex_str_len[sp_idx];
                ps_ginit_begin(sl_gi);
                ci = 0;
                while (ci < slen && ci < count) {
                    ps_ginit_emit_byte(sp[ci] & 255);
                    ci = ci + 1;
                }
                while (ci < count) {
                    ps_ginit_emit_byte(0);
                    ci = ci + 1;
                }
                ps_ginit_finish(sl_gi);
            } else if (neg) {
                ps_ginit_begin(sl_gi);
                if (lcols2 > 0) {
                    count = parse_global_init_array2d_at(ty,
                        (count >= 0) ? count / lcols2 : -1, lcols2, ps_gsub[sl_gi], sl_gi, 0);
                    count = count * lcols2;
                } else {
                    count = parse_global_init_array_at(ty, count, sl_gi, 0);
                }
                ps_gsize[sl_gi] = ty_size(ty) * count;
                ps_ginit_ensure_len(sl_gi, ps_gsize[sl_gi]);
                ps_ginit_finish(sl_gi);
            }
            if (ps_add_static_array_alias(nm, ty, lcols2, sl_gi) < 0) return nd_num(0);
            if (lex_tok != TK_COMMA) break;
            next();
            ty = ps_declarator_stars(base, base_cty);
            skip_decl_qualifiers();
            if (lex_tok != TK_IDENT) { p_error("expected name after comma"); return nd_num(0); }
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            ps_decl_inner = 0;
            if (lex_tok != TK_LBRACK) goto static_scalar_decl;
          }
            expect(TK_SEMI);
            return nd_block(NULL);
        }

        /* Everything else is a declarator list.  Each declarator brings
         * its own stars, dimensions and initializer; the statements
         * that initialize them run in declaration order. */
        head = NULL;
        tail = NULL;
        while (1) {
            t = parse_local_declarator(nm, ty);
            if (t != NULL) {
                if (head == NULL) head = t;
                else tail->next = t;
                tail = t;
                while (tail->next != NULL) tail = tail->next;
            }
            if (lex_tok != TK_COMMA) break;
            next();
            if (tdac > 0 && td_elem >= 0 && lex_tok != TK_STAR) {
                /* the next name is of the typedef'd array type too */
                ty = ps_declarator_stars(td_elem, 0);
                ps_decl_inner = tdac;
            } else {
                ty = ps_declarator_stars(base, base_cty);
            }
            skip_decl_qualifiers();
            if (lex_tok != TK_IDENT) break;
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            skip_gnu_decl_suffixes();
        }
        expect(TK_SEMI);
        if (head != NULL) return nd_block(head);
        return nd_block(NULL);
    }
    } /* end is_static scope */

    /* Label detection: identifier followed by colon */
    if (lex_tok == TK_IDENT) {
        memcpy(nm, lex_str, lex_slen + 1);
        ps_lex_mark();
        next();
        if (lex_tok == TK_COLON) {
            /* It's a label */
            ps_lex_commit();
            next();
            t = parse_stmt();
            return nd_label(ps_define_label(nm), t);
        }
        ps_lex_rewind();    /* not a label */
    }

    /* expression statement */
    n = parse_expr();
    expect(TK_SEMI);
    return nd_expr_stmt(n);
}

static Node *parse_block(void) {
    Node *head;
    Node *tail;
    Node *s;
    int saved_nlocals;
    int saved_stack;
    int saved_nconsts;
    int vtop;

    expect(TK_LBRACE);
    vtop = ps_vd_top;
    saved_nlocals = ps_scope_begin();
    saved_stack = ps_stack;
    saved_nconsts = ps_nconsts;
    ps_block_depth = ps_block_depth + 1;
    head = NULL;
    tail = NULL;
    while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
        s = parse_stmt();
        if (head == NULL) {
            head = s;
            tail = s;
        } else {
            tail->next = s;
            tail = s;
        }
    }
    expect(TK_RBRACE);
    head = ps_vla_block_end(head, tail, vtop);
    ps_scope_end(saved_nlocals);
    /* the block's enumerators and structure tags go out of scope with
     * it (6.2.1p4); they leaked, so an inner `enum { X = 100 }` was X
     * for the rest of the file, and an inner `struct t { ... }`
     * redefined the outer t (W11) */
    ps_nconsts = saved_nconsts;
    ps_end_block_tags();
    ps_block_depth = ps_block_depth - 1;
    /* a block's frame space goes back for the next block to use -- but
     * not once the function has called setjmp: a longjmp may come back
     * into this block after a later one has run, and what this one's
     * locals held must still be there */
    if (!ps_fn_setjmp) ps_stack = saved_stack;
    return nd_block(head);
}

/* --- Top-level parser --- */

/* Parse type + name + optional pointer stars + params.
 * Handles: function definitions, function prototypes, global variables. */
static void parse_type_and_stars(int *out_ty) {
    *out_ty = parse_type();
}

/* --- Old-style (K&R) definitions: int f(a, b) int a; char b; { ... } --- */

#define PS_KR_MAX 64
static char *ps_kr_names[PS_KR_MAX];   /* the identifier list, in order */
static int   ps_kr_ty[PS_KR_MAX];      /* each one's declared machine type; -1 = not declared */
static int   ps_kr_cty[PS_KR_MAX];
static int   ps_kr_n;

/* The identifier list (6.9.1p6), the cursor on its first name; left on
 * the ')'. */
static void ps_kr_read_ids(void) {
    ps_kr_n = 0;
    while (lex_tok == TK_IDENT) {
        if (ps_kr_n >= PS_KR_MAX) p_error("too many parameters");
        ps_kr_names[ps_kr_n] = strdup(lex_str);
        ps_kr_ty[ps_kr_n] = -1;
        ps_kr_n = ps_kr_n + 1;
        next();
        if (lex_tok != TK_COMMA) break;
        next();
    }
}

/* What an argument is in a call to a function without a prototype: the
 * default argument promotions (6.5.2.2p6) -- float to double, the small
 * integers to int.  The parameter arrives as that. */
static int ps_kr_promoted(int ty) {
    if (ty == TY_FLOAT) return TY_DOUBLE;
    if (!ty_is_ptr(ty) && !ty_is_struct(ty) && !ty_is_fp(ty) && ty_size(ty) < 4) return TY_INT;
    return ty;
}

/* The declaration list (6.9.1p6), the cursor after the identifier
 * list's ')': each name's type, then the parameters made in the list's
 * order -- each in its promoted type, and where that differs from the
 * declared one a local of the declared type assigned from it at entry,
 * those assignments in *pre.  Returns the parameter list; every name is
 * declared, and nothing else is (no implicit int since C99). */
static Node *ps_kr_declare(Node **pre) {
    int bty;
    int bcty;
    int ty;
    int k;
    int i;
    int off;
    int pt;
    Node *head;
    Node *tail;
    Node *p;
    Node *v;
    Node *a;
    Node *ptail;
    char hid[280];
    while (is_type()) {
        bty = parse_type();
        bcty = ps_type_nostar_cty;
        ty = bty;
        while (1) {
            if (lex_tok != TK_IDENT) p_error("expected parameter name in declaration list");
            k = 0;
            while (k < ps_kr_n && strcmp(ps_kr_names[k], lex_str) != 0) k = k + 1;
            if (k >= ps_kr_n) p_error("declaration of a name not in the identifier list");
            next();
            ps_parse_param_dims(&ty);       /* an array parameter is a pointer */
            ps_kr_ty[k] = ty;
            ps_kr_cty[k] = ps_type_cty;
            if (lex_tok != TK_COMMA) break;
            next();
            ty = ps_declarator_stars(ps_type_nostar, bcty);
        }
        expect(TK_SEMI);
    }
    head = NULL;
    tail = NULL;
    i = 0;
    while (i < ps_kr_n) {
        if (ps_kr_ty[i] < 0) p_error("parameter not declared in the declaration list");
        pt = ps_kr_promoted(ps_kr_ty[i]);
        if (pt == ps_kr_ty[i]) {
            ps_type_cty = ps_kr_cty[i];
            off = add_local(ps_kr_names[i], pt);
            p = nd_var(ps_kr_names[i], off, pt);
        } else {
            memcpy(hid, "__kr_", 5);
            strcpy(hid + 5, ps_kr_names[i]);
            ps_type_cty = 0;
            off = add_local(hid, pt);
            p = nd_var(hid, off, pt);
        }
        ps_bind_var(p, ps_nlocals - 1);
        if (head == NULL) head = p;
        else tail->next = p;
        tail = p;
        ps_nparams = ps_nparams + 1;
        i = i + 1;
    }
    /* the declared types, from the promoted arrivals */
    *pre = NULL;
    ptail = NULL;
    i = 0;
    p = head;
    while (i < ps_kr_n) {
        if (ps_kr_promoted(ps_kr_ty[i]) != ps_kr_ty[i]) {
            ps_type_cty = ps_kr_cty[i];
            off = add_local(ps_kr_names[i], ps_kr_ty[i]);
            v = nd_var(ps_kr_names[i], off, ps_kr_ty[i]);
            ps_bind_var(v, ps_nlocals - 1);
            memcpy(hid, "__kr_", 5);
            strcpy(hid + 5, ps_kr_names[i]);
            a = nd_var(hid, p->offset, p->ty);
            ps_bind_var(a, find_local(hid));
            a = nd_expr_stmt(nd_assign(v, nd_cast(a, ps_kr_ty[i])));
            if (*pre == NULL) *pre = a;
            else ptail->next = a;
            ptail = a;
        }
        p = p->next;
        i = i + 1;
    }
    return head;
}

/* A parameter declared as a function, int f(int), the cursor on its
 * '(': adjusted to a pointer to the function (6.7.6.3p8), as
 * int (*f)(int) is read -- the signature recorded, the local a word
 * typed as that pointer.  *pty becomes the word's machine type. */
static Node *ps_fn_typed_param(char *pnm, int *pty) {
    int base;
    int n;
    int ret_cty;
    int off;
    Node *p;
    ret_cty = ps_type_cty;
    next();
    base = ps_parse_fp_params(&n);
    ps_fp_set_ret(base, *pty);
    ps_fnptr_nstar = 1;
    ps_type_cty = ps_fnptr_cty(ret_cty);
    off = add_local(pnm, TY_INT);
    ps_lfpbase[ps_nlocals - 1] = base;
    ps_lfpn[ps_nlocals - 1] = n;
    p = nd_var(pnm, off, TY_INT);
    ps_bind_var(p, ps_nlocals - 1);
    p->is_fnptr = 1;
    p->val = base;
    p->nparams = n;
    *pty = TY_INT;
    return p;
}

/* A declaration whose declarator list goes on past a declarator its
 * own path does not continue from -- int x = 4, (*fp)(void), f(void);
 * (6.7p1) -- ends that call to parse_top_decl at the comma, keeping its
 * base type; the next call takes the rest from there instead of reading
 * specifiers. */
static int ps_decl_resume;
static int ps_dr_ty;
static int ps_dr_cty;
static int ps_dr_static;
static int ps_dr_extern;
static int ps_dr_const;
static int ps_dr_fpbase;
static int ps_dr_fpn;

static void ps_decl_keep(int ty, int cty, int is_static, int is_extern, int base_const) {
    while (ty_is_ptr(ty)) ty = ty_deref(ty);
    ps_dr_ty = ty;
    ps_dr_cty = cty;
    ps_dr_static = is_static;
    ps_dr_extern = is_extern;
    ps_dr_const = base_const;
    ps_dr_fpbase = ps_pending_retfpbase;
    ps_dr_fpn = ps_pending_retfpn;
}

/* The resumed declaration's type, its declarator's stars read. */
static int ps_decl_resumed(int *is_static, int *is_extern, int *base_const) {
    ps_decl_resume = 0;
    *is_static = ps_dr_static;
    *is_extern = ps_dr_extern;
    *base_const = ps_dr_const;
    ps_decl_objconst = 0;
    ps_pending_retfpbase = ps_dr_fpbase;
    ps_pending_retfpn = ps_dr_fpn;
    return ps_declarator_stars(ps_dr_ty, ps_dr_cty);
}

/* The end of a declarator: `;`, or `,` and the next one resumed. */
static void ps_decl_more(void) {
    if (lex_tok == TK_COMMA) {
        next();
        ps_decl_resume = 1;
        return;
    }
    expect(TK_SEMI);
}

/* After a comma, a declarator that opens with ( is left to a resumed
 * call: (*fp)(void), (name). */
static int ps_decl_paren_next(void) {
    if (lex_tok != TK_LPAREN) return 0;
    ps_decl_resume = 1;
    return 1;
}

/* A file-scope T (*name)[N], the cursor on the `[`: a pointer to an
 * array of N T, as a local of that type is -- its machine type T *, the
 * row width beside it.  It was refused at the `[`. */
static void ps_global_rowptr(char *nm, int ty, int is_static, int is_extern) {
    int cols;
    int idx;
    cols = ps_inner_dims(0, &ps_dims_sub);     /* T (*name)[M][K]: rows of rows */
    if (is_extern) idx = add_extern_global(nm, ty + TY_PTR, 0);
    else           idx = add_defined_global(nm, ty + TY_PTR, 0);
    if (is_static) ps_glocal[idx] = 1;
    ps_set_gcols(idx, cols);
    ps_gcty[idx] = ct_ptr(ps_rows_cty(ct_from_mty(ty, 0), cols, ps_gsub[idx]));
    if (lex_tok == TK_ASSIGN) {
        next();
        ps_scalar_global_init(idx, ty + TY_PTR);
    }
    ps_decl_more();
}

/* The outer declarator of T (*name(params))..., the cursor after its
 * `)`: (params) makes the return a function pointer (SQLite's
 * memdbDlSym), [N] a pointer to an array of N T (6.7.6.1).  The machine
 * type is a word pointer either way; the C type says how far a step
 * moves. */
static int ps_fn_ret_outer(int ret_cty) {
    int cols;
    if (lex_tok == TK_LBRACK) {
        next();
        cols = parse_const_int();
        expect(TK_RBRACK);
        return ct_ptr(ct_array(ret_cty, cols));
    }
    expect(TK_LPAREN);
    p_skip_to_rparen();
    return ct_ptr(ct_func(ret_cty, 0, -1, 0));
}

/* A function's declarator after its name, at the '(': the parameters,
 * then a prototype's ';' or the body.  Split from parse_top_decl, whose
 * size stage07 compiles only so far (selfhost ISSUES-100), so that the
 * parameter list may grow -- K&R definitions, function-typed
 * parameters.  nm and ty the declarator's name and type (the return
 * type), ret_cty its C type; fn_ret_fnptr when the function returns a
 * function pointer, T (*name(params))(params). */
static Node *ps_function_decl(char *nm, int ty, int ret_cty, int is_static, int fn_ret_fnptr) {
    Node *fn;
    Node *kr_pre;
    int kr;
    int noproto;        /* f(): no prototype (6.7.6.3p14), not f(void) */
    Node *phead;
    Node *ptail;
    Node *p;
    char pnm[256];
    int pcols;
    int pty;
    int fp_tys[64];
    int fp_ctys[64];
    int fp_n;
    int off;
    int i;
    int saw_unnamed_param;
    fp_n = 0;
    expect(TK_LPAREN);

    /* Reset locals for this function */
    i = 0;
    while (i < ps_nlocals) {
        free(ps_lname[i]);
        if (ps_lsname[i]) free(ps_lsname[i]);
        i = i + 1;
    }
    ps_nlocals = 0;
    ps_fn_setjmp = 0;
    ps_stack = 8;  /* reserve 8 bytes: saved r31 + saved r30 */
    ps_stack_max = 8;
    ps_fn_align = 0;
    ps_nvd = 0;
    ps_vd_top = -1;
    ps_nvgoto = 0;
    ps_nvpp = 0;
    ps_nvpend = 0;
    ps_fn_vm = 0;
    ps_slot_gen = 0;
    ps_nparams = 0;
    ps_is_varargs = 0;
    ps_struct_ret = 0;
    ps_retptr_off = 0;
    ps_nlabels = 0;
    saw_unnamed_param = 0;

    /* Hidden first param for struct return */
    if (ty_is_struct(ty)) {
        require_complete_type(ty, "incomplete return type");
        ps_struct_ret = 1;
        ps_retptr_off = add_local("__retptr", TY_PTR + TY_INT);
        ps_nparams = 1;
    }

    /* Parameters */
    phead = NULL;
    ptail = NULL;
    kr = 0;
    kr_pre = NULL;
    noproto = lex_tok == TK_RPAREN;
    if (lex_tok == TK_IDENT && !is_type()) {
        ps_kr_read_ids();       /* an old-style identifier list */
        kr = 1;
        goto params_done;
    }
    if (lex_tok != TK_RPAREN) {
        /* first param — or (void) meaning no params */
        if (!is_type()) {
            p_error("expected type in params");
            return NULL;
        }
        if (lex_tok == TK_VOID) {
            pty = parse_type();
            skip_decl_qualifiers();
            /* (void) → no params; parse_type takes the stars, so (void *)
             * also ends at the ')' -- and was recorded as no parameters */
            if (lex_tok == TK_RPAREN && pty == TY_VOID) {
                goto params_done;
            }
        } else {
            pty = parse_type();
            skip_decl_qualifiers();
        }
        pty = ps_declarator_stars(pty, ps_type_cty);
        /* Function pointer param: type (*name)(args) */
        if (lex_tok == TK_LPAREN) {
            int nested_fp;
            next();
            ps_skip_fnptr_stars();   /* (**name)(...) */
            /* T (*(*name)(params))(params), as in a struct member: the
             * inner ( * name ) ( params ) here, the outer ) ( params ) below */
            nested_fp = 0;
            if (lex_tok == TK_LPAREN) {
                next();
                ps_skip_fnptr_stars();   /* (**name)(...) */
                nested_fp = 1;
            }
            ps_type_cty = ps_fnptr_cty(nested_fp ? ct_ptr(ct_func(ps_type_cty, 0, -1, 0)) : ps_type_cty);
            if (lex_tok == TK_IDENT) {
                off = add_local(lex_str, TY_INT);
                p = nd_var(lex_str, off, TY_INT);
                ps_bind_var(p, ps_nlocals - 1);
                next();
            } else {
                /* an unnamed function-pointer parameter in a prototype:
                 * int (*)(void*,int) (SQLite's API declarations) */
                p = NULL;
                saw_unnamed_param = 1;
            }
            if (nested_fp) {
                expect(TK_RPAREN);
                expect(TK_LPAREN);
                p_skip_to_rparen();
            }
            expect(TK_RPAREN);
            if (lex_tok == TK_LBRACK && !nested_fp) {
                /* T (*name)[M]: a pointer to a row of M elements -- the
                 * spelled-out form of `T name[][M]`.  Retype the local
                 * (added above as a word) and record the row width. */
                pcols = ps_inner_dims(0, &ps_dims_sub);   /* T (*name)[M][K]: rows of rows */
                pty = pty + TY_PTR;
                if (p != NULL) {
                    ps_ltype[ps_nlocals - 1] = pty;
                    ps_lcols[ps_nlocals - 1] = pcols;
                    ps_lsub[ps_nlocals - 1] = ps_dims_sub;
                    ps_lcty[ps_nlocals - 1] = ct_ptr(ps_rows_cty(ct_from_mty(ty_deref(pty), 0),
                                                                 pcols, ps_dims_sub));
                    p->ty = pty;
                    p->arr_cols = pcols;
                    p->arr_sub = ps_dims_sub;
                    p->cty = ps_lcty[ps_nlocals - 1];
                }
            } else {
            if (lex_tok == TK_LPAREN) {
                next();
                p_skip_to_rparen();
            }
            pty = TY_INT;  /* record fn-ptr param as a word */
            }
        } else if (lex_tok == TK_LBRACK) {
            ps_parse_param_dims(&pty);
            p = NULL;
            saw_unnamed_param = 1;
        } else if (lex_tok != TK_IDENT) {
            if (lex_tok == TK_COMMA || lex_tok == TK_RPAREN) {
                p = NULL;
                saw_unnamed_param = 1;
            } else {
                p_error("expected param name");
                return NULL;
            }
        } else {
            require_complete_type(pty, "incomplete parameter type");
            memcpy(pnm, lex_str, lex_slen + 1);
            next();
            if (lex_tok == TK_LPAREN) {
                p = ps_fn_typed_param(pnm, &pty);
            } else {
            pcols = ps_parse_param_dims(&pty);
            off = add_local(pnm, pty);      /* ps_parse_param_dims adjusted ps_type_cty */
            ps_lcols[ps_nlocals - 1] = pcols;
            ps_lsub[ps_nlocals - 1] = ps_dims_sub;
            p = nd_var(pnm, off, pty);
            ps_bind_var(p, ps_nlocals - 1);
            }
        }
        skip_gnu_decl_suffixes();
        if (fp_n < 64) { fp_tys[fp_n] = pty; fp_ctys[fp_n] = (p != NULL && p->cty > 0) ? p->cty : ps_cty_quiet(pty); fp_n = fp_n + 1; }
        ps_nparams = 1;
        phead = p;
        ptail = p;

        while (lex_tok == TK_COMMA) {
            next();
            if (lex_tok == TK_ELLIPSIS) {
                ps_is_varargs = 1;
                next();
                break;
            }
            if (!is_type()) {
                p_error("expected type in params");
                return NULL;
            }
            pty = parse_type();
            skip_decl_qualifiers();
            pty = ps_declarator_stars(pty, ps_type_cty);
            /* Function pointer param: type (*name)(args) */
            if (lex_tok == TK_LPAREN) {
                int nested_fp;
                int fp_ret_ty;
                fp_ret_ty = pty;  /* T in T (*name)(params) */
                next();
                ps_skip_fnptr_stars();   /* (**name)(...) */
                /* T (*(*name)(params))(params), as in a struct member: the
                 * inner ( * name ) ( params ) here, the outer ) ( params ) below */
                nested_fp = 0;
                if (lex_tok == TK_LPAREN) {
                    next();
                    ps_skip_fnptr_stars();   /* (**name)(...) */
                    nested_fp = 1;
                }
                ps_type_cty = ps_fnptr_cty(nested_fp ? ct_ptr(ct_func(ps_type_cty, 0, -1, 0)) : ps_type_cty);
                if (lex_tok == TK_IDENT) {
                off = add_local(lex_str, TY_INT);
                    p = nd_var(lex_str, off, TY_INT);
                    ps_bind_var(p, ps_nlocals - 1);
                    next();
                } else {
                    /* an unnamed function-pointer parameter in a prototype:
                     * int (*)(void*,int) (SQLite's API declarations) */
                    p = NULL;
                    saw_unnamed_param = 1;
                }
                if (nested_fp) {
                    expect(TK_RPAREN);
                    expect(TK_LPAREN);
                    p_skip_to_rparen();
                    fp_ret_ty = TY_PTR + TY_INT;
                }
                expect(TK_RPAREN);
                if (lex_tok == TK_LBRACK && !nested_fp) {
                    /* T (*name)[M]: a pointer to a row of M elements -- the
                     * spelled-out form of `T name[][M]`.  Retype the local
                     * (added above as a word) and record the row width. */
                    pcols = ps_inner_dims(0, &ps_dims_sub);   /* T (*name)[M][K]: rows of rows */
                    pty = pty + TY_PTR;
                    if (p != NULL) {
                        ps_ltype[ps_nlocals - 1] = pty;
                        ps_lcols[ps_nlocals - 1] = pcols;
                        ps_lsub[ps_nlocals - 1] = ps_dims_sub;
                        ps_lcty[ps_nlocals - 1] = ct_ptr(ps_rows_cty(ct_from_mty(ty_deref(pty), 0),
                                                                     pcols, ps_dims_sub));
                        p->ty = pty;
                        p->arr_cols = pcols;
                        p->arr_sub = ps_dims_sub;
                        p->cty = ps_lcty[ps_nlocals - 1];
                    }
                } else {
                if (lex_tok == TK_LPAREN) {
                    int pfpb;
                    int pfn;
                    next();
                    if (nested_fp) {
                        p_skip_to_rparen();
                    } else {
                        /* Record the signature so a call through this
                         * parameter keeps the return type (GitHub issue 69)
                         * and converts i64 args (GitHub issue 41). */
                        pfpb = ps_parse_fp_params(&pfn);
                        ps_fp_set_ret(pfpb, fp_ret_ty);
                        if (p != NULL) {
                            ps_lfpbase[ps_nlocals - 1] = pfpb;
                            ps_lfpn[ps_nlocals - 1] = pfn;
                            p->is_fnptr = 1;
                            p->val = pfpb;
                            p->nparams = pfn;
                        }
                    }
                }
                pty = TY_INT;  /* record fn-ptr param as a word */
                }
            } else if (lex_tok == TK_LBRACK) {
                ps_parse_param_dims(&pty);
                p = NULL;
                saw_unnamed_param = 1;
            } else if (lex_tok != TK_IDENT) {
                if (lex_tok == TK_COMMA || lex_tok == TK_RPAREN) {
                    p = NULL;
                    saw_unnamed_param = 1;
                } else {
                    p_error("expected param name");
                    return NULL;
                }
            } else {
                require_complete_type(pty, "incomplete parameter type");
                memcpy(pnm, lex_str, lex_slen + 1);
                next();
                if (lex_tok == TK_LPAREN) {
                    p = ps_fn_typed_param(pnm, &pty);
                } else {
                pcols = ps_parse_param_dims(&pty);
                off = add_local(pnm, pty);
                ps_lcols[ps_nlocals - 1] = pcols;
                ps_lsub[ps_nlocals - 1] = ps_dims_sub;
                p = nd_var(pnm, off, pty);
                ps_bind_var(p, ps_nlocals - 1);
                }
            }
            skip_gnu_decl_suffixes();
            if (phead == NULL) {
                phead = p;
                ptail = p;
            } else if (p != NULL) {
                ptail->next = p;
                ptail = p;
            }
            if (fp_n < 64) { fp_tys[fp_n] = pty; fp_ctys[fp_n] = (p != NULL && p->cty > 0) ? p->cty : ps_cty_quiet(pty); fp_n = fp_n + 1; }
            ps_nparams = ps_nparams + 1;
        }
    }
params_done:
    expect(TK_RPAREN);
    if (fn_ret_fnptr) {
        expect(TK_RPAREN);
        ret_cty = ps_fn_ret_outer(ret_cty);
    }
    skip_gnu_decl_suffixes();

    /* Prototype: type name(params); */
    if (lex_tok == TK_SEMI || lex_tok == TK_COMMA) {
        if (kr) p_error("a parameter list without types is only allowed in a function definition");
        ps_decl_more();
        add_func_type(nm, ty, fp_tys, fp_ctys, noproto ? -1 : fp_n,
                      ret_cty);
        return NULL;
    }

    /* Function body */
    if (saw_unnamed_param) {
        p_error("expected param name");
        return NULL;
    }
    if (kr) {
        phead = ps_kr_declare(&kr_pre);
        fp_n = -1;      /* no prototype: calls promote their arguments */
    }
    if (noproto) fp_n = -1;
    add_func_type(nm, ty, fp_tys, fp_ctys, fp_n,
                  ret_cty);
    fn = nd_new(ND_FUNC);
    fn->name = strdup(nm);
    fn->ty = ty;  /* store return type for sema pass */
    fn->args = phead;
    fn->nparams = ps_nparams;
    fn->is_varargs = ps_is_varargs;
    fn->is_static = is_static;
    fn->offset = ps_struct_ret ? ps_retptr_off : 0; /* hidden __retptr offset */
    ps_cur_func = fn->name;
    ps_in_body = 1;
    kr_pre = ps_vla_param_pre(kr_pre);
    fn->body = parse_block();
    ps_vla_gotos();
    ps_in_body = 0;
    if (kr_pre) {           /* the K&R parameters' conversions come first */
        p = kr_pre;
        while (p->next) p = p->next;
        p->next = fn->body->body;
        fn->body->body = kr_pre;
    }
    fn->locals_size = ps_stack_max;
    fn->bit_width = ps_fn_align;
    fn->arr_cols = ps_fn_vm;        /* not inlined: hl_inl_sel_one */

    return fn;
}

static Node *parse_top_decl(void) {
    int fp_is_arr;
    int fp_count;
    int fp_nstars;
    int fp_has_params;
    int fn_ret_fnptr;
    Node *fn;
    Node *phead;
    Node *ptail;
    Node *p;
    char nm[256];
    char pnm[256];
    int pcols;
    int ty;
    int pty;
    int fp_tys[64];
    int fp_ctys[64];    /* the parameters' C types, beside fp_tys */
    int ret_cty;        /* the declared type's C type: a function's return type */
    int fp_n;
    int off;
    int i;
    int count;
    int g2cols;
    int gtdac;
    int was_enum;
    int neg;
    int idx;
    int si;
    int base;
    int nf;
    int nsi;
    int nnf;
    int nj;
    int sp_idx;
    int slen;
    int saw_unnamed_param;
    int xty;
    int is_extern;
    int is_static;
    int decl_had_init;
    int lead_const;     /* const among the storage-class words */
    int gbase_cty;      /* the base C type, for the declarators after a comma */
    int base_const;     /* the declaration's base type is const */
    int decl_const;     /* the current declarator's object is const */
    char *sp;

    /* _Static_assert(expr, "message") — compile-time check, no codegen */
    if (lex_tok == TK_STATIC_ASSERT) {
        int sa_val;
        Node *sa_expr;
        next();
        expect(TK_LPAREN);
        sa_expr = parse_assign();
        /* Evaluate constant expression — only supports ND_NUM for now */
        sa_val = ps_static_assert_value(sa_expr);
        expect(TK_COMMA);
        while (lex_tok == TK_STRING) next(); /* message may be adjacent literals */
        expect(TK_RPAREN);
        expect(TK_SEMI);
        if (sa_val == 0) p_error("_Static_assert failed");
        return NULL;  /* no AST node emitted */
    }

    /* Storage class / qualifier keywords.  We honour `static` on
     * function definitions (emit STB_LOCAL); other qualifiers (const,
     * inline, register, restrict, auto, GNU attrs) have no semantic
     * effect in this single-file compiler. */
    fp_n = 0;
    if (ps_decl_resume) {
        ty = ps_decl_resumed(&is_static, &is_extern, &base_const);
        lead_const = base_const;
        ret_cty = ps_type_cty;
        gbase_cty = ps_dr_cty;
        was_enum = 0;
        g2cols = 0;
        gtdac = 0;
        goto declarators;
    }
    is_extern = 0;
    is_static = 0;
    lead_const = 0;
    base_const = 0;
    decl_const = 0;
    ps_decl_objconst = 0;
    skip_gnu_attributes();
    while (lex_tok == TK_STATIC || lex_tok == TK_CONST ||
           lex_tok == TK_EXTERN || lex_tok == TK_INLINE ||
           lex_tok == TK_REGISTER || lex_tok == TK_RESTRICT ||
           lex_tok == TK_AUTO || is_gnu_qual_ident() ||
           is_gnu_extension_ident() || is_gnu_inline_ident() ||
           is_gnu_attr_ident()) {
        if (lex_tok == TK_EXTERN) is_extern = 1;
        if (lex_tok == TK_STATIC) is_static = 1;
        if (lex_tok == TK_CONST) lead_const = 1;
        if (is_gnu_attr_ident()) skip_gnu_attributes();
        else ps_storage_next();
    }

    /* Typedef */
    if (lex_tok == TK_TYPEDEF) {
        next();
        parse_typedef_decl();
        return NULL;
    }


    /* Parse return type / variable type */
    was_enum = (lex_tok == TK_ENUM);
    ty = parse_type();
    ret_cty = ps_type_cty;
    g2cols = ps_type_arrcount;  /* reuse: typedef'd array element count */
    gtdac = 0;
    ps_pending_retfpbase = ps_type_fpbase;
    ps_pending_retfpn = ps_type_fpn;
    /* Whether the object each declarator names is itself const decides
     * .rodata against .data (selfhost ISSUES-99).  Saved now: an
     * initializer's cast or sizeof runs parse_type again. */
    base_const = lead_const || ps_type_constb;
    gbase_cty = ps_type_nostar_cty;
declarators:
    ps_decl_keep(ty, gbase_cty, is_static, is_extern, base_const);
    ps_skipq_const = 0;
    skip_decl_qualifiers();
    if (ps_type_nstar > 0) decl_const = ps_type_constp || ps_skipq_const;
    else                   decl_const = base_const || ps_skipq_const;

    /* Bare tag definition with no declarator: `struct Foo { ... };` or
       `enum { A, B };`.  An enum decays to TY_INT, so the type alone
       cannot tell it from a declarator-less `int;` -- hence the token
       captured above. */
    if (lex_tok == TK_SEMI && (ty_is_struct(ty) || was_enum)) {
        next();
        return NULL;
    }

    /* Global function pointer: type (*name)(args); */
    fn_ret_fnptr = 0;
    fp_is_arr = 0;
    fp_count = -1;
    if (lex_tok == TK_LPAREN) {
        next();
        if (ps_try_grouping_paren()) goto plain_name;
        decl_const = 0;     /* (*name)(...), (*name[N]): left writable */
        fp_nstars = 0;
        while (lex_tok == TK_STAR) {         /* (**name)(...) */
            fp_nstars = fp_nstars + 1;
            next();
        }
        skip_decl_qualifiers();              /* (*const name)(...) */
        if (lex_tok != TK_IDENT) {
            p_error("expected name in fn ptr decl");
            return NULL;
        }
        memcpy(nm, lex_str, lex_slen + 1);
        next();
        if (lex_tok == TK_LBRACK) {
            /* an array of function pointers: (*const tab[])(sqlite3*) = {..}
             * -- laid out as an int array whose words are symbol relocations */
            next();
            fp_is_arr = 1;
            if (lex_tok != TK_RBRACK) fp_count = parse_const_int();
            expect(TK_RBRACK);
        }
        if (lex_tok == TK_LPAREN) {
            /* T (*name(params))(params): a function returning a function
             * pointer (SQLite's memdbDlSym).  The return type is an opaque
             * pointer; the outer ) (params) is taken after the parameters. */
            fn_ret_fnptr = 1;
            ty = TY_PTR + TY_INT;
            goto function_decl;
        }
        expect(TK_RPAREN);
        if (lex_tok == TK_LBRACK && !fp_is_arr && fp_nstars == 1) {
            ps_global_rowptr(nm, ty, is_static, is_extern);
            return NULL;
        }
        fp_has_params = 0;
        fp_n = 0;
        fp_tys[0] = -1;
        if (lex_tok == TK_LPAREN) {
            fp_has_params = 1;
            next();
            fp_tys[0] = ps_parse_fp_params(&fp_n);
            ps_fp_set_ret(fp_tys[0], ty);
        }
        skip_gnu_decl_suffixes();
        /* T (*name[N]) with no (params) is T *name[N], not an array of
         * function pointers (GitHub issue 47).  SQLite's shell writes
         * char *(azHelp[]) (star outside, grouping hack above); the
         * ordinary spelling is char (*azHelp[]). */
        if (fp_is_arr && !fp_has_params) {
            i = 0;
            while (i < fp_nstars) {
                ty = ty + TY_PTR;
                i = i + 1;
            }
            xty = ty;
            count = fp_count;
            g2cols = 0;
            goto array_after_brackets;
        }
        if (fp_is_arr) {
            /* the declaration's type, for the redeclaration check below
             * to compare: left stale, extern int (*tab[])(int); and its
             * definition were "a different type" */
            /* no size and no initializer: an extern reference or a
             * tentative definition, one element, as any array's (with a
             * size of 0 it was taken for a pointer, and a use loaded the
             * table's first word as its address) */
            if (fp_count < 0 && lex_tok != TK_ASSIGN) fp_count = ps_tentative_count(is_static);
            ps_fnptr_nstar = fp_nstars;
            ps_type_cty = ct_array(ps_fnptr_cty(ret_cty), fp_count);
            if (lex_tok == TK_ASSIGN) {
                next();
                if (lex_tok != TK_LBRACE) { p_error("expected { in fn ptr array init"); return NULL; }
                idx = add_defined_global(nm, TY_INT + TY_PTR, 0);
                if (is_static) ps_glocal[idx] = 1;
                ps_ginit_begin(idx);
                fp_count = parse_global_init_array_at(TY_INT, fp_count, idx, 0);
                ps_gsize[idx] = 4 * fp_count;
                ps_ginit_ensure_len(idx, ps_gsize[idx]);
                ps_ginit_finish(idx);
            } else {
                if (is_extern) idx = add_extern_global(nm, TY_INT + TY_PTR, 4 * fp_count);
                else           idx = add_defined_global(nm, TY_INT + TY_PTR, 4 * fp_count);
                if (is_static) ps_glocal[idx] = 1;
            }
            ps_fnptr_nstar = fp_nstars;     /* initialized or not: methods[2] had int elements */
            ps_gcty[idx] = ct_array(ps_fnptr_cty(ret_cty), fp_count);
            ps_gfpbase[idx] = fp_tys[0];
            ps_gfpn[idx] = fp_n;
            ps_decl_more();
            return NULL;
        }
        ps_fnptr_nstar = fp_nstars;
        ps_type_cty = ps_fnptr_cty(ret_cty);
        if (is_extern) idx = add_extern_global(nm, TY_INT, 0);
        else           idx = add_defined_global(nm, TY_INT, 0);
        if (is_static) ps_glocal[idx] = 1; /* file-scope static: TU-local */
        ps_gfpbase[idx] = fp_tys[0];
        ps_gfpn[idx] = fp_n;
        if (lex_tok == TK_ASSIGN) {
            /* fn-ptr global initializer: NULL/0 or a function name
             * (sbasic: env_deftype_hook = NULL), or &name, the same
             * address (6.6p9; a leading & can only be address-of) */
            next();
            if (lex_tok == TK_AMP) next();
            if (lex_tok == TK_IDENT && find_const(lex_str) < 0 &&
                (is_known_func(lex_str) || find_global(lex_str) >= 0)) {
                ps_ginit_begin(idx);
                parse_global_init_symbol_reloc_at(idx, 0, 4);
                ps_ginit_ensure_len(idx, 4);
                ps_ginit_finish(idx);
            } else {
                ps_set_scalar_init(idx, parse_const_int(), 0);
            }
        }
        ps_decl_more();
        return NULL;
    }

    /* Name */
plain_name:
    if (lex_tok != TK_IDENT) {
        p_error("expected name in declaration");
        return NULL;
    }
    memcpy(nm, lex_str, lex_slen + 1);
    next();
    skip_gnu_decl_suffixes();

    /* Typedef'd array global: sha1_digest_t g;  — real array storage. */
    if (g2cols > 0 && lex_tok == TK_SEMI) {
        ps_decl_objconst = decl_const;
        if (is_extern) idx = add_extern_global(nm, ty, ty_size(ty_deref(ty)) * g2cols);
        else           idx = add_defined_global(nm, ty, ty_size(ty_deref(ty)) * g2cols);
        if (is_static) ps_glocal[idx] = 1;
        ps_gcols[idx] = 0;
        ps_gsub[idx] = 0;
        next();
        return NULL;
    }
    gtdac = g2cols;         /* a typedef'd array type's own dimension, if it is one */
    g2cols = 0;
    xty = ty;
    /* an object of a typedef'd array type is the array (A x = {...};
     * went to the scalars, its braces an "unexpected token") */
    if (gtdac > 0 && lex_tok != TK_LBRACK) { xty = ty_deref(ty); count = gtdac; goto array_after_brackets; }

    /* Global declarators: a comma-separated list of scalars and arrays --
     * `int a, b[3], *c, d[2][4];` (GitHub #31: an array declarator used
     * to end the list). Each pass handles one kind and, on a comma, reads
     * the next name and goes round. */
    while (lex_tok == TK_SEMI || lex_tok == TK_ASSIGN || lex_tok == TK_COMMA || lex_tok == TK_LBRACK) {
    /* Global scalar(s): type name; type name = expr; or type a, b = expr; */
    if (lex_tok == TK_SEMI || lex_tok == TK_ASSIGN || lex_tok == TK_COMMA) {
        while (1) {
            decl_had_init = (lex_tok == TK_ASSIGN);
            ps_decl_objconst = decl_const;
            if (ty_is_struct(xty)) {
                /* extern struct pt pv; may precede pt's completion (6.7p7) */
                if (!is_extern || decl_had_init) require_complete_type(xty, "incomplete global type");
                if (is_extern && !decl_had_init)
                    idx = add_extern_global(nm, xty, ty_size(xty));
                else
                    idx = add_defined_global(nm, xty, ty_size(xty));
                    if (is_static) ps_glocal[idx] = 1; /* file-scope static: TU-local */
            } else {
                if (is_extern && !decl_had_init)
                    idx = add_extern_global(nm, xty, 0);
                else
                    idx = add_defined_global(nm, xty, 0);
                    if (is_static) ps_glocal[idx] = 1; /* file-scope static: TU-local */
            }
            skip_gnu_decl_suffixes();
            if (lex_tok == TK_ASSIGN) {
                next();
                ps_init_check(ps_gcty[idx], xty);
                if (lex_tok == TK_LBRACE && ty_is_struct(xty)) {
                    /* Struct initializer: struct S s = { ... }; */
                    ps_ginit_begin(idx);
                    parse_global_init_value(xty, idx);
                    ps_ginit_finish(idx);
                } else if (lex_tok == TK_STRING && !ps_string_then_offset()) {
                    ps_set_str_init(idx, parse_string_literal());
                } else if (ty_is_fp(xty)) {
                    /* float/double global initializer: FP literal or
                     * integer constant, converted at full precision
                     * (ps_fp_init_store_at). */
                    ps_ginit_begin(idx);
                    ps_fp_init_store_at(xty, idx, 0);
                    ps_ginit_ensure_len(idx, ty_size(xty));
                    ps_ginit_finish(idx);
                } else if (ty_is_ptr(xty) &&
                           (lex_tok == TK_AMP ||
                            (lex_tok == TK_IDENT && find_const(lex_str) < 0 &&
                             (find_global(lex_str) >= 0 || ps_static_local_sym(lex_str))))) {
                    /* Pointer global initialized with an address constant:
                     * `const fixed_t *finecosine = &finesine[FINEANGLES/4];`
                     * Route through the init-pool reloc machinery. */
                    ps_ptr_init_at(idx, xty);
                } else {
                    if (ty_is_llong(xty)) {
                        /* a long long global: the literal's own high word */
                        int glo;
                        int ghi;
                        ghi = parse_const_ll_hi(&glo);
                        ps_set_scalar_init(idx, glo, ghi);
                    } else {
                        ps_scalar_global_init(idx, xty);
                    }
                }
            }
            if (lex_tok != TK_COMMA) break;
            next();
            if (ps_decl_paren_next()) return NULL;
            xty = ty;
            while (ty_is_ptr(xty)) xty = ty_deref(xty);
            xty = ps_declarator_stars(xty, gbase_cty);
            ps_skipq_const = 0;
            skip_decl_qualifiers();
            if (ps_type_nstar > 0) decl_const = ps_type_constp || ps_skipq_const;
            else                   decl_const = base_const || ps_skipq_const;
            if (lex_tok != TK_IDENT) {
                p_error("expected name in declaration");
                return NULL;
            }
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            skip_gnu_decl_suffixes();
            if (lex_tok == TK_LPAREN) goto list_function;
            if (lex_tok == TK_LBRACK) break;        /* an array declarator: the branch below */
        }
        if (lex_tok == TK_LBRACK) continue;
        expect(TK_SEMI);
        return NULL;
    }
    if (lex_tok == TK_LBRACK) {
        next();
        count = -1;
        if (lex_tok != TK_RBRACK) {
            count = ps_array_size();
        }
        expect(TK_RBRACK);
        /* A multi-dimensional global: flattened, the elements the first
         * subscript steps over recorded so it scales by a whole row, and
         * past two dimensions the strides below (ps_dims_sub).  The first
         * dimension may be omitted when an initializer infers it
         * (T a[][C] = {...}).  An array of a typedef'd array type --
         * jmp_buf handlers[4] -- has the typedef's dimension innermost.
         * (It was taken for an array of pointers: sixteen bytes for four
         * jmp_bufs.) */
        g2cols = ps_inner_dims((gtdac > 0 && xty == ty) ? gtdac : 0, &ps_dims_sub);
        if (gtdac > 0 && xty == ty) xty = ty_deref(ty);
        if (count >= 0 && g2cols > 0) count = count * g2cols;
        skip_gnu_decl_suffixes();
array_after_brackets:
        ps_decl_objconst = decl_const;
        /* An array's C type before it is registered: an initializer may
         * fix its length after add_*_global has run (T a[] = {...}), and
         * a size-0 registration would look like a pointer. */
        ps_type_cty = ps_array_cty(ps_cty_quiet(xty), count, g2cols, g2cols > 0 ? ps_dims_sub : 0);
        if (g2cols > 0 && lex_tok == TK_ASSIGN) {
            next();
            if (lex_tok != TK_LBRACE) {
                p_error("expected { in 2D array init");
                return NULL;
            }
            require_complete_type(xty, "incomplete element type");
            idx = add_defined_global(nm, xty + TY_PTR, 0);
            if (is_static) ps_glocal[idx] = 1; /* file-scope static: TU-local */
            ps_set_gcols(idx, g2cols);
            ps_ginit_begin(idx);
            count = parse_global_init_array2d_at(xty, (count >= 0) ? count / g2cols : -1,
                                                 g2cols, ps_gsub[idx], idx, 0);
            count = count * g2cols;
            ps_gsize[idx] = ty_size(xty) * count;
            ps_fix_global_array(idx, count, g2cols);
            ps_ginit_ensure_len(idx, ps_gsize[idx]);
            ps_ginit_finish(idx);
        } else if (lex_tok == TK_ASSIGN) {
            next();
            ps_braced_string_open(ps_is_char_ty(xty));
            if (lex_tok == TK_STRING) {
                /* String array init: char s[N] = "str" or char s[] = "str"
                 * (with adjacent-string concatenation via parse_string_literal). */
                sp_idx = parse_string_literal();
                ps_braced_string_close();
                slen = lex_str_len[sp_idx];
                ps_init_string_fits(sp_idx, count);
                if (count < 0) count = slen + 1;
                require_complete_type(xty, "incomplete element type");
                idx = add_defined_global(nm, xty + TY_PTR, ty_size(xty) * count);
                if (is_static) ps_glocal[idx] = 1; /* file-scope static: TU-local */
                ps_ginit_begin(idx);
                /* parse_string_literal already consumed the literal, so emit
                 * its bytes directly rather than calling the aggregate
                 * initializer parser (which would try to re-parse a string
                 * that's no longer at the lexer cursor). */
                sp = lex_strpool + lex_str_off[sp_idx];
                i = 0;
                while (i < slen && i < count) {
                    ps_ginit_emit_byte(sp[i] & 255);
                    i = i + 1;
                }
                while (i < count) {
                    ps_ginit_emit_byte(0);
                    i = i + 1;
                }
                ps_ginit_finish(idx);
            } else if (lex_tok == TK_LBRACE) {
                idx = add_defined_global(nm, xty + TY_PTR, 0);
                if (is_static) ps_glocal[idx] = 1; /* file-scope static: TU-local */
                ps_ginit_begin(idx);
                count = parse_global_init_array_at(xty, count, idx, 0);
                require_complete_type(xty, "incomplete element type");
                ps_gsize[idx] = ty_size(xty) * count;
                ps_fix_global_array(idx, count, 0);
                ps_ginit_ensure_len(idx, ps_gsize[idx]);
                ps_ginit_finish(idx);
            } else {
                p_error("expected string or { in array init");
                return NULL;
            }
        } else {
            /* an extern declaration, or a tentative definition: one element
             * unless a later declaration completes it (6.9.2p2) */
            if (count < 0) count = ps_tentative_count(is_static);
            require_complete_type(xty, "incomplete element type");
            if (is_extern) idx = add_extern_global(nm, xty + TY_PTR, ty_size(xty) * count);
            else           idx = add_defined_global(nm, xty + TY_PTR, ty_size(xty) * count);
            if (is_static) ps_glocal[idx] = 1; /* file-scope static: TU-local */
            ps_set_gcols(idx, g2cols);
            ps_fix_global_array(idx, count, g2cols);   /* its rows, as an initialized one has */
        }
        if (lex_tok != TK_COMMA) {
            expect(TK_SEMI);
            return NULL;
        }
        /* another declarator in the same declaration */
        next();
        if (ps_decl_paren_next()) return NULL;
        xty = ty;
        while (ty_is_ptr(xty)) xty = ty_deref(xty);
        xty = ps_declarator_stars(xty, gbase_cty);
        ps_skipq_const = 0;
        skip_decl_qualifiers();
        if (ps_type_nstar > 0) decl_const = ps_type_constp || ps_skipq_const;
        else                   decl_const = base_const || ps_skipq_const;
        if (lex_tok != TK_IDENT) {
            p_error("expected name in declaration");
            return NULL;
        }
        memcpy(nm, lex_str, lex_slen + 1);
        next();
        skip_gnu_decl_suffixes();
        if (lex_tok == TK_LPAREN) goto list_function;
        continue;
    }
    }

    /* Function: type name(params) { body } or type name(params); */
function_decl:
    return ps_function_decl(nm, ty, ret_cty, is_static, fn_ret_fnptr);
    /* f(void) after a comma: a function declarator in the list */
list_function:
    return ps_function_decl(nm, xty, ps_type_cty, is_static, 0);

}

static Node *parse_program(void) {
    Node *prog;
    Node *fhead;
    Node *ftail;
    Node *f;
    int usize_ty;
    int isize_ty;

    ps_nglobals = 0;
    ps_ginit_pool_len = 0;
    ps_ngirelocs = 0;
    ps_comp_lit_id = 0;
    ct_init();
    pp_install_predefs();
#ifdef S12CC_TARGET_A64
    pp_add("__aarch64__", 1);
    pp_add("__LP64__", 1);
#endif
#ifdef S12CC_TARGET_X64
    pp_add("__x86_64__", 1);
    pp_add("__LP64__", 1);
#endif
#ifndef S12CC_TARGET_A64
#ifndef S12CC_TARGET_X64
    /* clang's names for the target (and stage01 cc.fth's): code that
     * keys off them -- mdfix's shims, s32sort's heap sizing -- took the
     * host branch under stage08. */
    pp_add("__SLOW32__", 1);
    pp_add("__slow32__", 1);
#endif
#endif
    if (ty_ptr_size == 8) {
        pp_add("__SIZEOF_POINTER__", 8);
    } else {
        pp_add("__SIZEOF_POINTER__", 4);
    }
    if (ty_ptr_size == 8) {
        usize_ty = TY_LLONG | TY_UNSIGNED;
        isize_ty = TY_LLONG;
    } else {
        usize_ty = TY_INT | TY_UNSIGNED;
        isize_ty = TY_INT;
    }
    add_typedef("va_list", TY_PTR + TY_CHAR);
    add_typedef("__builtin_va_list", TY_PTR + TY_CHAR);
    /* C99 _Bool / C++ bool: 1 byte unsigned per the standard.  Matches
     * gcc/clang and lets cc-x64-compiled structs layout-match anything
     * else linked into the same binary.  Was TY_INT for a long time,
     * which silently corrupted any struct containing a bool field. */
    add_typedef("_Bool", TY_CHAR | TY_UNSIGNED);
    ps_tdcty[ps_ntypedefs - 1] = ct_basic[CT_BOOL];
    add_typedef("bool", TY_CHAR | TY_UNSIGNED);
    ps_tdcty[ps_ntypedefs - 1] = ct_basic[CT_BOOL];
    add_typedef("size_t", usize_ty);
    add_typedef("ptrdiff_t", isize_ty);
    add_typedef("intptr_t", isize_ty);
    add_typedef("uintptr_t", usize_ty);
    /* C99 fixed-width integer typedefs.  All hosts (SLOW-32 native,
     * x64, AArch64) agree: int=32, long long=64, short=16, char=8. */
    add_typedef("int8_t",   TY_CHAR);
    add_typedef("uint8_t",  TY_CHAR | TY_UNSIGNED);
    add_typedef("int16_t",  TY_SHORT);
    add_typedef("uint16_t", TY_SHORT | TY_UNSIGNED);
    add_typedef("int32_t",  TY_INT);
    add_typedef("uint32_t", TY_INT | TY_UNSIGNED);
    add_typedef("int64_t",  TY_LLONG);
    add_typedef("uint64_t", TY_LLONG | TY_UNSIGNED);
    add_typedef("__int128", TY_I128);
    add_typedef("__int128_t", TY_I128);
    add_typedef("__uint128_t", TY_I128 | TY_UNSIGNED);
    ps_typedefs_builtin();
    next();  /* prime the first token */

    fhead = NULL;
    ftail = NULL;
    while (lex_tok != TK_EOF) {
        /* A stray `;` at file scope: SQLite's shell has `SQLITE_EXTENSION_INIT1;`
         * with the macro empty.  Every compiler accepts it. */
        if (lex_tok == TK_SEMI) { next(); continue; }
        f = parse_top_decl();
        if (f && f->kind == ND_FUNC) ps_func_defined(f->name);
        if (f) {
            if (fhead == NULL) {
                fhead = f;
                ftail = f;
            } else {
                ftail->next = f;
                ftail = f;
            }
        }
    }

    prog = nd_new(ND_PROGRAM);
    prog->body = fhead;
    return prog;
}
