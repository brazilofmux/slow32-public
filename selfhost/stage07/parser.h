/* parser.h -- Recursive-descent parser for stage04 compiler
 *
 * Phase 2: types, pointers, strings, globals, local arrays.
 * Builds a typed AST from the Ragel lexer token stream.
 * Compiled by stage03 s32-cc.
 */

/* Forward declarations (needed for host GCC compilation of cross-compiler) */
static int find_typedef(char *name);
static Node *parse_unary(void);

/* --- Shared label counter (used by both parser and codegen) --- */
static int cg_lbl;    /* label counter (monotonically increasing) */

static int cg_label(void) {
    int l;
    l = cg_lbl;
    cg_lbl = cg_lbl + 1;
    return l;
}

/* --- Parser state --- */
#define P_MAX_LOCALS  128
#define P_MAX_GLOBALS 8192
#define PS_MAX_CONSTS 512

static char *ps_lname[P_MAX_LOCALS];  /* local var names */
static int   ps_loff[P_MAX_LOCALS];   /* local var offsets from fp */
static int   ps_ltype[P_MAX_LOCALS];  /* local var types */
static int   ps_larr[P_MAX_LOCALS];   /* 1 if array (addr, no load) */
static int   ps_lsize[P_MAX_LOCALS];  /* total byte size (arrays: elem_sz*count) */
static int   ps_lstatic[P_MAX_LOCALS]; /* 1 = static local, 0 = normal */
static char *ps_lsname[P_MAX_LOCALS];  /* mangled name (static locals only) */
static int   ps_nlocals;
static int   ps_stack;                /* current stack allocation */
static int   ps_nparams;              /* params in current func */
static int   ps_is_varargs;           /* 1 if current func has ... */
static int   ps_struct_ret;           /* 1 if current func returns struct via hidden ptr */
static int   ps_retptr_off;           /* stack offset of hidden __retptr param */

static char *ps_gname[P_MAX_GLOBALS]; /* global var names */
static int   ps_gtype[P_MAX_GLOBALS]; /* global var types */
static int   ps_gsize[P_MAX_GLOBALS]; /* size in bytes (0=scalar, >0=array) */
static int   ps_ginit[P_MAX_GLOBALS]; /* initial value for scalars */
static int   ps_ginit_hi[P_MAX_GLOBALS]; /* hi word for 64-bit global initializers */
static int   ps_gstr[P_MAX_GLOBALS];  /* string init: pool index, -1 if none */
static int   ps_glocal[P_MAX_GLOBALS]; /* 1 = static local (suppress .global) */
static int   ps_nglobals;

/* Static local variable state */
static char *ps_cur_func;             /* current function name, NULL outside */
static char  ps_sl_buf[256];          /* scratch buffer for name mangling */
static int   ps_sl_count;             /* global static-local counter */

/* Array/struct initializer pool for globals */
#define PS_MAX_INIT_POOL 8192
static int ps_ginit_pool[PS_MAX_INIT_POOL];
static int ps_ginit_start[P_MAX_GLOBALS]; /* -1 = no init list */
static int ps_ginit_count[P_MAX_GLOBALS]; /* number of values */
static int ps_ginit_pool_len;

/* goto/label table (per-function, reset at each function) */
#define P_MAX_LABELS 512
static char *ps_lblname[P_MAX_LABELS];
static int   ps_lblid[P_MAX_LABELS];
static int   ps_nlabels;

/* Enum constant table */
static char *ps_cname[PS_MAX_CONSTS];
static int   ps_cval[PS_MAX_CONSTS];
static int   ps_nconsts;

/* Typedef table */
#define PS_MAX_TYPEDEFS 128
static char *ps_tdname[PS_MAX_TYPEDEFS];
static int   ps_tdtype[PS_MAX_TYPEDEFS];
static int   ps_ntypedefs;

/* Function return type table (for 64-bit return value tracking) */
#define PS_MAX_FUNCS 256
static char *ps_fname[PS_MAX_FUNCS];
static int   ps_ftype[PS_MAX_FUNCS];
static int   ps_nfuncs;

/* Forward declarations */
static Node *parse_expr(void);
static Node *parse_stmt(void);
static Node *parse_assign(void);
static Node *parse_postfix(void);

/* --- Utilities --- */

static void next(void) {
    int di;
    while (1) {
        lex_next();
        if (lex_tok == TK_HASH) { pp_directive(); continue; }
        if (pp_skip) { continue; }
        if (lex_tok == TK_IDENT) {
            di = pp_find(lex_str);
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
                return;
            }
        }
        return;
    }
}

static void p_error(char *msg) {
    fdputs("s12cc:", 2);
    fdputuint(2, lex_line);
    fdputs(": error: ", 2);
    fdputs(msg, 2);
    fdputc(10, 2);
    exit(1);
}

static void expect(int tok) {
    if (lex_tok != tok) {
        fdputs("s12cc:", 2);
        fdputuint(2, lex_line);
        fdputs(": expected token ", 2);
        fdputuint(2, tok);
        fdputs(" got ", 2);
        fdputuint(2, lex_tok);
        fdputc(10, 2);
        exit(1);
    }
    next();
}

static int is_type(void) {
    if (lex_tok == TK_INT) return 1;
    if (lex_tok == TK_VOID) return 1;
    if (lex_tok == TK_CHAR) return 1;
    if (lex_tok == TK_STRUCT) return 1;
    if (lex_tok == TK_UNION) return 1;
    if (lex_tok == TK_UNSIGNED) return 1;
    if (lex_tok == TK_SIGNED) return 1;
    if (lex_tok == TK_LONG) return 1;
    if (lex_tok == TK_SHORT) return 1;
    if (lex_tok == TK_FLOAT) return 1;
    if (lex_tok == TK_DOUBLE) return 1;
    if (lex_tok == TK_CONST) return 1;
    if (lex_tok == TK_VOLATILE) return 1;
    if (lex_tok == TK_IDENT && find_typedef(lex_str) >= 0) return 1;
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
    ps_tdtype[ps_ntypedefs] = ty;
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

static void add_func_type(char *name, int ty) {
    int i;
    /* Update existing entry if already registered */
    i = ps_nfuncs - 1;
    while (i >= 0) {
        if (strcmp(name, ps_fname[i]) == 0) {
            ps_ftype[i] = ty;
            return;
        }
        i = i - 1;
    }
    if (ps_nfuncs >= PS_MAX_FUNCS) return;
    ps_fname[ps_nfuncs] = strdup(name);
    ps_ftype[ps_nfuncs] = ty;
    ps_nfuncs = ps_nfuncs + 1;
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
    ps_lblid[ps_nlabels] = cg_label();
    ps_nlabels = ps_nlabels + 1;
    return ps_lblid[ps_nlabels - 1];
}

static void parse_enum_def(void) {
    int val;
    /* Skip optional tag name */
    if (lex_tok == TK_IDENT) {
        next();
    }
    expect(TK_LBRACE);
    val = 0;
    while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
        if (lex_tok != TK_IDENT) {
            p_error("expected enum constant name");
        }
        if (ps_nconsts >= PS_MAX_CONSTS) {
            p_error("too many enum constants");
        }
        ps_cname[ps_nconsts] = strdup(lex_str);
        next();
        if (lex_tok == TK_ASSIGN) {
            next();
            /* Parse optional negative sign */
            if (lex_tok == TK_MINUS) {
                next();
                if (lex_tok != TK_NUM) {
                    p_error("expected number after '-' in enum");
                }
                val = 0 - lex_val;
                next();
            } else if (lex_tok == TK_NUM) {
                val = lex_val;
                next();
            } else {
                p_error("expected number in enum initializer");
            }
        }
        ps_cval[ps_nconsts] = val;
        ps_nconsts = ps_nconsts + 1;
        val = val + 1;
        if (lex_tok == TK_COMMA) {
            next();
        }
    }
    expect(TK_RBRACE);
}

/* --- Struct helpers --- */

static int find_struct(char *name) {
    int i;
    i = st_count - 1;
    while (i >= 0) {
        if (strcmp(name, st_name[i]) == 0) return i;
        i = i - 1;
    }
    return -1;
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
    st_is_union[idx] = 0;
    st_count = st_count + 1;
    return idx;
}

static int find_member(int sty, char *name) {
    int si;
    int base;
    int nf;
    int i;
    si = ty_struct_idx(sty);
    base = st_first[si];
    nf = st_nfields[si];
    i = 0;
    while (i < nf) {
        if (strcmp(name, stm_name[base + i]) == 0) return base + i;
        i = i + 1;
    }
    return -1;
}

/* Parse a type: int, char, void, struct, with optional pointer stars */
static int parse_type(void) {
    int ty;
    int si;
    int mty;
    int off;
    int max_sz;
    int arr_count;
    char nm[256];
    /* Skip const/volatile/signed/restrict qualifiers */
    while (lex_tok == TK_CONST || lex_tok == TK_VOLATILE ||
           lex_tok == TK_SIGNED || lex_tok == TK_RESTRICT) next();
    if (lex_tok == TK_INT)  { ty = TY_INT;  next(); }
    else if (lex_tok == TK_CHAR) { ty = TY_CHAR; next(); }
    else if (lex_tok == TK_VOID) { ty = TY_VOID; next(); }
    else if (lex_tok == TK_SHORT) {
        next();
        if (lex_tok == TK_INT) { ty = TY_SHORT; next(); }
        else { ty = TY_SHORT; }
    }
    else if (lex_tok == TK_UNSIGNED) {
        next();
        if (lex_tok == TK_CHAR) { ty = TY_CHAR | TY_UNSIGNED; next(); }
        else if (lex_tok == TK_SHORT) { ty = TY_SHORT | TY_UNSIGNED; next(); }
        else if (lex_tok == TK_INT) { ty = TY_INT | TY_UNSIGNED; next(); }
        else if (lex_tok == TK_LONG) {
            next();
            if (lex_tok == TK_LONG) {
                ty = TY_LLONG | TY_UNSIGNED; next();
            } else {
                ty = TY_INT | TY_UNSIGNED;
            }
            if (lex_tok == TK_INT) next();
        }
        else { ty = TY_INT | TY_UNSIGNED; }
    }
    else if (lex_tok == TK_LONG) {
        next();
        if (lex_tok == TK_LONG) { ty = TY_LLONG; next(); if (lex_tok == TK_INT) next(); }
        else if (lex_tok == TK_INT) { ty = TY_INT; next(); }
        else if (lex_tok == TK_UNSIGNED) { ty = TY_INT | TY_UNSIGNED; next(); }
        else if (lex_tok == TK_DOUBLE) { ty = TY_DOUBLE; next(); }
        else { ty = TY_INT; }
    }
    else if (lex_tok == TK_FLOAT) { ty = TY_FLOAT; next(); }
    else if (lex_tok == TK_DOUBLE) { ty = TY_DOUBLE; next(); }
    else if (lex_tok == TK_STRUCT) {
        next();
        if (lex_tok != TK_IDENT) {
            p_error("expected struct tag name");
            return TY_INT;
        }
        memcpy(nm, lex_str, lex_slen + 1);
        next();
        si = find_struct(nm);
        if (lex_tok == TK_LBRACE) {
            /* Struct definition: struct Name { ... } */
            next();
            if (si < 0) {
                si = add_struct(nm);
            }
            off = 0;
            while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                mty = parse_type();
                if (lex_tok != TK_IDENT) {
                    p_error("expected member name");
                    return TY_INT;
                }
                /* Align offset: char=1, short=2, else=4 */
                if ((mty & TY_BASE_MASK) == TY_CHAR) {
                    /* 1-byte aligned */
                } else if ((mty & TY_BASE_MASK) == TY_SHORT) {
                    off = ((off + 1) / 2) * 2;
                } else {
                    off = ((off + 3) / 4) * 4;
                }
                if (stm_count >= ST_MAX_MEMBERS) {
                    p_error("too many struct members");
                    return TY_INT;
                }
                stm_name[stm_count] = strdup(lex_str);
                next();
                /* Check for array member: type name[N]; */
                arr_count = 0;
                if (lex_tok == TK_LBRACK) {
                    next();
                    if (lex_tok != TK_NUM) {
                        p_error("array size required in struct member");
                        return TY_INT;
                    }
                    arr_count = lex_val;
                    next();
                    expect(TK_RBRACK);
                }
                if (arr_count > 0) {
                    stm_type[stm_count] = mty + TY_PTR;
                    stm_is_arr[stm_count] = 1;
                    stm_arr_size[stm_count] = ty_size(mty) * arr_count;
                    stm_off[stm_count] = off;
                    off = off + ty_size(mty) * arr_count;
                } else {
                    stm_type[stm_count] = mty;
                    stm_is_arr[stm_count] = 0;
                    stm_arr_size[stm_count] = 0;
                    stm_off[stm_count] = off;
                    off = off + ty_size(mty);
                }
                stm_count = stm_count + 1;
                st_nfields[si] = st_nfields[si] + 1;
                expect(TK_SEMI);
            }
            expect(TK_RBRACE);
            /* Round total size to multiple of 4 */
            st_size[si] = ((off + 3) / 4) * 4;
        } else {
            /* Forward reference: struct Name (no brace) */
            if (si < 0) {
                p_error("undefined struct");
                return TY_INT;
            }
        }
        ty = TY_STRUCT_BASE + si;
    }
    else if (lex_tok == TK_UNION) {
        next();
        if (lex_tok != TK_IDENT) {
            p_error("expected union tag name");
            return TY_INT;
        }
        memcpy(nm, lex_str, lex_slen + 1);
        next();
        si = find_struct(nm);
        if (lex_tok == TK_LBRACE) {
            /* Union definition: union Name { ... } */
            next();
            if (si < 0) {
                si = add_struct(nm);
            }
            st_is_union[si] = 1;
            max_sz = 0;
            while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                mty = parse_type();
                if (lex_tok != TK_IDENT) {
                    p_error("expected member name");
                    return TY_INT;
                }
                if (stm_count >= ST_MAX_MEMBERS) {
                    p_error("too many struct members");
                    return TY_INT;
                }
                stm_name[stm_count] = strdup(lex_str);
                next();
                /* Check for array member: type name[N]; */
                arr_count = 0;
                if (lex_tok == TK_LBRACK) {
                    next();
                    if (lex_tok != TK_NUM) {
                        p_error("array size required in union member");
                        return TY_INT;
                    }
                    arr_count = lex_val;
                    next();
                    expect(TK_RBRACK);
                }
                if (arr_count > 0) {
                    stm_type[stm_count] = mty + TY_PTR;
                    stm_is_arr[stm_count] = 1;
                    stm_arr_size[stm_count] = ty_size(mty) * arr_count;
                    stm_off[stm_count] = 0;
                    if (ty_size(mty) * arr_count > max_sz) max_sz = ty_size(mty) * arr_count;
                } else {
                    stm_type[stm_count] = mty;
                    stm_is_arr[stm_count] = 0;
                    stm_arr_size[stm_count] = 0;
                    stm_off[stm_count] = 0;
                    if (ty_size(mty) > max_sz) max_sz = ty_size(mty);
                }
                stm_count = stm_count + 1;
                st_nfields[si] = st_nfields[si] + 1;
                expect(TK_SEMI);
            }
            expect(TK_RBRACE);
            /* Round total size to multiple of 4 */
            st_size[si] = ((max_sz + 3) / 4) * 4;
        } else {
            /* Forward reference: union Name (no brace) */
            if (si < 0) {
                p_error("undefined union");
                return TY_INT;
            }
        }
        ty = TY_STRUCT_BASE + si;
    }
    else if (lex_tok == TK_IDENT) {
        ty = find_typedef(lex_str);
        if (ty < 0) { p_error("expected type"); return TY_INT; }
        next();
    }
    else { p_error("expected type"); return TY_INT; }
    while (lex_tok == TK_STAR) { ty = ty + TY_PTR; next(); }
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

static int add_local(char *name, int ty) {
    int idx;
    int sz;
    if (ps_nlocals >= P_MAX_LOCALS) {
        p_error("too many locals");
        return 0;
    }
    sz = ty_size(ty);
    /* Round up to multiple of 4 */
    sz = ((sz + 3) / 4) * 4;
    ps_stack = ps_stack + sz;
    idx = ps_nlocals;
    ps_lname[idx] = strdup(name);
    ps_loff[idx] = 0 - ps_stack;
    ps_ltype[idx] = ty;
    ps_larr[idx] = 0;
    ps_lsize[idx] = sz;
    ps_lstatic[idx] = 0;
    ps_lsname[idx] = NULL;
    ps_nlocals = ps_nlocals + 1;
    return ps_loff[idx];
}

/* Add a local array. Returns offset of start of array. */
static int add_local_array(char *name, int elem_ty, int count) {
    int idx;
    int total;
    int elem_sz;
    if (ps_nlocals >= P_MAX_LOCALS) {
        p_error("too many locals");
        return 0;
    }
    elem_sz = ty_size(elem_ty);
    total = elem_sz * count;
    /* Round up to multiple of 4 */
    total = ((total + 3) / 4) * 4;
    ps_stack = ps_stack + total;
    idx = ps_nlocals;
    ps_lname[idx] = strdup(name);
    ps_loff[idx] = 0 - ps_stack;
    ps_ltype[idx] = elem_ty + TY_PTR;  /* array decays to pointer */
    ps_larr[idx] = 1;
    ps_lsize[idx] = total;
    ps_nlocals = ps_nlocals + 1;
    return ps_loff[idx];
}

static int add_global(char *name, int ty, int size_bytes) {
    int idx;
    if (ps_nglobals >= P_MAX_GLOBALS) {
        p_error("too many globals");
        return 0;
    }
    idx = ps_nglobals;
    ps_gname[idx] = strdup(name);
    ps_gtype[idx] = ty;
    ps_gsize[idx] = size_bytes;
    ps_ginit_hi[idx] = 0;
    ps_gstr[idx] = -1;
    ps_ginit_start[idx] = -1;
    ps_ginit_count[idx] = 0;
    ps_glocal[idx] = 0;
    ps_nglobals = ps_nglobals + 1;
    return idx;
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

/* Parse a compile-time constant integer (for global initializers) */
static int parse_const_int(void) {
    int neg;
    int ci;
    neg = 0;
    if (lex_tok == TK_MINUS) { neg = 1; next(); }
    if (lex_tok == TK_NUM) {
        ci = lex_val;
        next();
        return neg ? (0 - ci) : ci;
    }
    if (lex_tok == TK_IDENT) {
        ci = find_const(lex_str);
        if (ci >= 0) {
            ci = ps_cval[ci];
            next();
            return neg ? (0 - ci) : ci;
        }
    }
    p_error("expected constant integer in initializer");
    return 0;
}

/* --- Expression parser (operator precedence climbing) --- */

static Node *parse_primary(void) {
    Node *n;
    int v;
    int ty;
    char nm[256];
    Node *head;
    Node *tail;
    Node *arg;
    int nargs;
    int li;
    int gi;
    int ci;

    /* Number literal */
    if (lex_tok == TK_NUM) {
        v = lex_val;
        next();
        return nd_num(v);
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
        next();
        return nd_num(v);
    }

    /* String literal */
    if (lex_tok == TK_STRING) {
        v = lex_val;  /* string pool index */
        next();
        return nd_string(v);
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
                n->is_local = 0;
                n->is_array = 0;
                return n;
            }
            n = nd_var(nm, ps_loff[li], ps_ltype[li]);
            n->is_local = 1;
            n->is_array = ps_larr[li];
            return n;
        }

        /* Global variable */
        gi = find_global(nm);
        if (gi >= 0) {
            n = nd_var(nm, 0, ps_gtype[gi]);
            n->is_local = 0;
            n->is_array = (ps_gsize[gi] > 0) ? 1 : 0;
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
            while (lex_tok == TK_STAR) { ty = ty + TY_PTR; next(); }
            expect(TK_RPAREN);
            n = nd_new(ND_VA_ARG);
            n->lhs = arg;
            n->ty = ty;
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
            n = nd_call(nm, head, nargs);
            n->ty = find_func_type(nm);
            return n;
        }

        /* Bare function name: load address (for function pointers) */
        return nd_func_ref(nm);
    }

    /* Parenthesized expression or type cast */
    if (lex_tok == TK_LPAREN) {
        next();
        if (is_type()) {
            ty = parse_type();
            expect(TK_RPAREN);
            n = parse_unary();
            return nd_cast(n, ty);
        }
        n = parse_expr();
        expect(TK_RPAREN);
        return n;
    }

    /* sizeof(type_or_expr) */
    if (lex_tok == TK_SIZEOF) {
        next();
        expect(TK_LPAREN);
        if (is_type()) {
            v = ty_size(parse_type());
        } else {
            n = parse_expr();
            if (n->kind == ND_VAR && n->is_array) {
                /* Local array: use stored total size */
                if (n->is_local) {
                    li = find_local(n->name);
                    v = (li >= 0) ? ps_lsize[li] : ty_size(n->ty);
                } else {
                    gi = find_global(n->name);
                    v = (gi >= 0) ? ps_gsize[gi] : ty_size(n->ty);
                }
            } else if (n->kind == ND_MEMBER && n->is_array) {
                /* Struct array member: arr_size stored in val_hi */
                v = n->val_hi;
            } else {
                v = ty_size(n->ty);
            }
        }
        expect(TK_RPAREN);
        return nd_num(v);
    }

    p_error("unexpected token in expression");
    return nd_num(0);
}

/* Postfix: handle array subscript p[i], postfix ++/--, member access . and -> */
static Node *parse_postfix(void) {
    Node *n;
    Node *idx;
    Node *pi;
    Node *ahead;
    Node *atail;
    Node *aarg;
    int anargs;
    int sty;
    int mi;
    char mnm[256];

    n = parse_primary();
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
            n = nd_call_ptr(n, ahead, anargs);
        } else if (lex_tok == TK_LBRACK) {
            next();
            idx = parse_expr();
            expect(TK_RBRACK);
            /* n[idx] → *(n + idx)  — codegen handles pointer arithmetic scaling */
            n = nd_binop(TK_PLUS, n, idx);
            n = nd_unary(TK_STAR, n);
        } else if (lex_tok == TK_DOT) {
            next();
            if (lex_tok != TK_IDENT) {
                p_error("expected member name after '.'");
            }
            memcpy(mnm, lex_str, lex_slen + 1);
            next();
            sty = n->ty;
            if (!ty_is_struct(sty)) {
                p_error("'.' on non-struct type");
            }
            mi = find_member(sty, mnm);
            if (mi < 0) {
                p_error("undefined struct member");
            }
            n = nd_member(n, stm_off[mi], stm_type[mi], stm_is_arr[mi], stm_arr_size[mi]);
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
            n = nd_member(n, stm_off[mi], stm_type[mi], stm_is_arr[mi], stm_arr_size[mi]);
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

static Node *parse_unary(void) {
    Node *n;

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
        next();
        n = parse_unary();
        return nd_unary(TK_STAR, n);
    }
    /* Address-of */
    if (lex_tok == TK_AMP) {
        next();
        n = parse_unary();
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
        return nd_assign(n, parse_assign());
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

/* --- Statement parser --- */

static Node *parse_block(void);

static Node *parse_stmt(void) {
    Node *n;
    Node *c;
    Node *t;
    Node *e;
    Node *head;
    Node *tail;
    Node *a;
    int ty;
    int off;
    int count;
    int ci;
    int cv;
    int neg;
    int si;
    int base;
    int nf;
    int nsi;
    int nnf;
    int nj;
    int mi_off;
    int mi_ty;
    int xty;
    int sp_idx;
    int slen;
    char *sp;
    char nm[256];
    /* Lexer save/restore for label lookahead */
    int sv_tok; int sv_val; int sv_slen; int sv_rcs; int sv_ract;
    char *sv_rp; char *sv_rts; char *sv_rte;
    char sv_str[256];

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
        return nd_do_while(c, t);
    }

    /* while statement */
    if (lex_tok == TK_WHILE) {
        next();
        expect(TK_LPAREN);
        c = parse_expr();
        expect(TK_RPAREN);
        t = parse_stmt();
        return nd_while(c, t);
    }

    /* for statement (first-class node) */
    if (lex_tok == TK_FOR) {
        next();
        expect(TK_LPAREN);
        ci = ps_nlocals;  /* save scope for for-init declarations */
        /* init */
        if (lex_tok == TK_SEMI) {
            next();
            n = NULL;
        } else if (is_type()) {
            /* for-loop init declaration: for (int i = 0; ...) */
            ty = parse_type();
            if (lex_tok != TK_IDENT) {
                p_error("expected identifier in for-init");
                return nd_num(0);
            }
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            off = add_local(nm, ty);
            if (lex_tok == TK_ASSIGN) {
                next();
                n = nd_assign(nd_var(nm, off, ty), parse_expr());
                n->lhs->is_local = 1;
            } else {
                n = NULL;
            }
            expect(TK_SEMI);
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
        ps_nlocals = ci;  /* restore scope */
        return nd_for(n, c, e, t);
    }

    /* switch statement */
    if (lex_tok == TK_SWITCH) {
        next();
        expect(TK_LPAREN);
        c = parse_expr();
        expect(TK_RPAREN);
        t = parse_stmt();
        n = nd_new(ND_SWITCH);
        n->cond = c;
        n->body = t;
        return n;
    }

    /* case label */
    if (lex_tok == TK_CASE) {
        next();
        neg = 0;
        if (lex_tok == TK_MINUS) {
            neg = 1;
            next();
        }
        if (lex_tok == TK_NUM) {
            cv = lex_val;
            next();
        } else if (lex_tok == TK_IDENT) {
            ci = find_const(lex_str);
            if (ci < 0) {
                p_error("undefined constant in case");
            }
            cv = ps_cval[ci];
            next();
        } else {
            p_error("expected constant in case");
            cv = 0;
        }
        if (neg) cv = 0 - cv;
        expect(TK_COLON);
        n = nd_new(ND_CASE);
        n->val = cv;
        return n;
    }

    /* default label */
    if (lex_tok == TK_DEFAULT) {
        next();
        expect(TK_COLON);
        return nd_new(ND_DEFAULT);
    }

    /* break statement */
    if (lex_tok == TK_BREAK) {
        next();
        expect(TK_SEMI);
        return nd_new(ND_BREAK);
    }

    /* continue statement */
    if (lex_tok == TK_CONTINUE) {
        next();
        expect(TK_SEMI);
        return nd_new(ND_CONTINUE);
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
        return nd_goto(find_or_add_label(nm));
    }

    /* block */
    if (lex_tok == TK_LBRACE) {
        return parse_block();
    }

    /* enum definition inside function body */
    if (lex_tok == TK_ENUM) {
        next();
        parse_enum_def();
        expect(TK_SEMI);
        return nd_block(NULL);
    }

    /* Track static qualifier before local declarations */
    {
    int is_static;
    int sl_gi;
    int sl_li;
    is_static = 0;
    while (lex_tok == TK_STATIC || lex_tok == TK_CONST ||
           lex_tok == TK_REGISTER || lex_tok == TK_RESTRICT ||
           lex_tok == TK_AUTO) {
        if (lex_tok == TK_STATIC) is_static = 1;
        next();
    }

    /* local variable declaration */
    if (is_type()) {
        ty = parse_type();
        /* Function pointer declaration: type (*name)(args); */
        if (lex_tok == TK_LPAREN) {
            next();
            if (lex_tok == TK_STAR) next();
            if (lex_tok != TK_IDENT) {
                p_error("expected identifier in fn ptr decl");
                return nd_num(0);
            }
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            expect(TK_RPAREN);
            /* Skip parameter list */
            if (lex_tok == TK_LPAREN) {
                next();
                while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next();
                expect(TK_RPAREN);
            }
            off = add_local(nm, TY_INT);
            if (lex_tok == TK_ASSIGN) {
                next();
                n = nd_assign(nd_var(nm, off, TY_INT), parse_expr());
                n->lhs->is_local = 1;
                expect(TK_SEMI);
                return nd_expr_stmt(n);
            }
            expect(TK_SEMI);
            return nd_block(NULL);
        }
        if (lex_tok != TK_IDENT) {
            p_error("expected identifier in declaration");
            return nd_num(0);
        }
        memcpy(nm, lex_str, lex_slen + 1);
        next();

        /* Static local scalar: emit as global with mangled name */
        if (is_static && lex_tok != TK_LBRACK) {
            ps_mangle_static(ps_cur_func, nm);
            sl_gi = add_global(ps_sl_buf, ty, 0);
            ps_glocal[sl_gi] = 1;
            if (lex_tok == TK_ASSIGN) {
                next();
                ps_ginit[sl_gi] = parse_const_int();
            }
            expect(TK_SEMI);
            /* Add to local table for scoped name lookup */
            sl_li = ps_nlocals;
            if (sl_li >= P_MAX_LOCALS) { p_error("too many locals"); return nd_num(0); }
            ps_lname[sl_li] = strdup(nm);
            ps_loff[sl_li] = 0;
            ps_ltype[sl_li] = ty;
            ps_larr[sl_li] = 0;
            ps_lstatic[sl_li] = 1;
            ps_lsname[sl_li] = strdup(ps_sl_buf);
            ps_nlocals = ps_nlocals + 1;
            return nd_block(NULL);
        }

        /* Array declaration: type name[N]; or type name[N] = ...; or type name[] = ...; */
        if (lex_tok == TK_LBRACK) {
            next();
            count = -1;
            if (lex_tok == TK_NUM) {
                count = lex_val;
                next();
            }
            expect(TK_RBRACK);
            head = NULL;
            if (lex_tok == TK_ASSIGN) {
                next();
                if (lex_tok == TK_STRING) {
                    /* String array init: char s[N] = "str" or char s[] = "str" */
                    sp_idx = lex_val;
                    slen = lex_str_len[sp_idx];
                    if (count < 0) count = slen + 1;
                    off = add_local_array(nm, ty, count);
                    sp = lex_strpool + lex_str_off[sp_idx];
                    tail = NULL;
                    ci = 0;
                    while (ci < slen && ci < count) {
                        n = nd_var(nm, off, ty + TY_PTR);
                        n->is_local = 1;
                        n->is_array = 1;
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
                        n->is_local = 1;
                        n->is_array = 1;
                        a = nd_binop(TK_PLUS, n, nd_num(ci));
                        a = nd_unary(TK_STAR, a);
                        a = nd_assign(a, nd_num(0));
                        a = nd_expr_stmt(a);
                        if (head == NULL) { head = a; tail = a; }
                        else { tail->next = a; tail = a; }
                    }
                    next();
                } else if (lex_tok == TK_LBRACE) {
                    next();
                    tail = NULL;
                    ci = 0;
                    /* First pass: parse expressions, count them */
                    /* Store expr nodes temporarily in the block list */
                    while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                        if (ci > 0) expect(TK_COMMA);
                        if (count >= 0 && ci >= count) {
                            p_error("too many initializers"); break;
                        }
                        /* Store parsed expr as ND_EXPR_STMT placeholder */
                        a = nd_expr_stmt(parse_assign());
                        if (head == NULL) { head = a; tail = a; }
                        else { tail->next = a; tail = a; }
                        ci = ci + 1;
                    }
                    expect(TK_RBRACE);
                    if (count < 0) count = ci;
                    off = add_local_array(nm, ty, count);
                    /* Second pass: wrap each expr into *(arr+i) = expr */
                    a = head;
                    head = NULL;
                    tail = NULL;
                    ci = 0;
                    while (a != NULL) {
                        n = nd_var(nm, off, ty + TY_PTR);
                        n->is_local = 1;
                        n->is_array = 1;
                        t = nd_binop(TK_PLUS, n, nd_num(ci));
                        t = nd_unary(TK_STAR, t);
                        t = nd_assign(t, a->lhs);
                        t = nd_expr_stmt(t);
                        if (head == NULL) { head = t; tail = t; }
                        else { tail->next = t; tail = t; }
                        a = a->next;
                        ci = ci + 1;
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
            }
            expect(TK_SEMI);
            if (head != NULL) return nd_block(head);
            return nd_block(NULL);
        }

        /* Scalar or struct: type name; or type name = expr; */
        off = add_local(nm, ty);
        head = NULL;
        tail = NULL;
        if (lex_tok == TK_ASSIGN) {
            next();
            if (lex_tok == TK_LBRACE && ty_is_struct(ty)) {
                /* Local struct initializer: struct S s = {v1, v2, ...}; */
                next();
                si = ty_struct_idx(ty);
                base = st_first[si];
                nf = st_nfields[si];
                ci = 0;  /* field index */
                while (ci < nf && lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                    if (ty_is_struct(stm_type[base + ci])) {
                        /* Nested struct: flatten */
                        nsi = ty_struct_idx(stm_type[base + ci]);
                        nnf = st_nfields[nsi];
                        nj = 0;
                        while (nj < nnf && lex_tok != TK_RBRACE) {
                            if (head != NULL) expect(TK_COMMA);
                            mi_off = stm_off[base + ci] + stm_off[st_first[nsi] + nj];
                            mi_ty = stm_type[st_first[nsi] + nj];
                            n = nd_var(nm, off, ty);
                            n->is_local = 1;
                            a = nd_member(n, mi_off, mi_ty, 0, 0);
                            a = nd_assign(a, parse_assign());
                            a = nd_expr_stmt(a);
                            if (head == NULL) { head = a; tail = a; }
                            else { tail->next = a; tail = a; }
                            nj = nj + 1;
                        }
                    } else {
                        if (head != NULL) expect(TK_COMMA);
                        mi_off = stm_off[base + ci];
                        mi_ty = stm_type[base + ci];
                        n = nd_var(nm, off, ty);
                        n->is_local = 1;
                        a = nd_member(n, mi_off, mi_ty, 0, 0);
                        a = nd_assign(a, parse_assign());
                        a = nd_expr_stmt(a);
                        if (head == NULL) { head = a; tail = a; }
                        else { tail->next = a; tail = a; }
                    }
                    ci = ci + 1;
                }
                expect(TK_RBRACE);
                expect(TK_SEMI);
                if (head != NULL) return nd_block(head);
                return nd_block(NULL);
            }
            n = nd_assign(nd_var(nm, off, ty), parse_expr());
            n->lhs->is_local = 1;
            t = nd_expr_stmt(n);
            if (head == NULL) { head = t; tail = t; }
            else { tail->next = t; tail = t; }
        }
        /* Additional declarators after comma: int a, b; or int *a, *b; */
        while (lex_tok == TK_COMMA) {
            next();
            /* Strip pointer depth from ty to get base type */
            xty = ty;
            while (ty_is_ptr(xty)) xty = ty_deref(xty);
            /* Each declarator adds its own pointer stars */
            while (lex_tok == TK_STAR) { xty = xty + TY_PTR; next(); }
            if (lex_tok != TK_IDENT) break;
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            off = add_local(nm, xty);
            if (lex_tok == TK_ASSIGN) {
                next();
                n = nd_assign(nd_var(nm, off, xty), parse_expr());
                n->lhs->is_local = 1;
                t = nd_expr_stmt(n);
                if (head == NULL) { head = t; tail = t; }
                else { tail->next = t; tail = t; }
            }
        }
        expect(TK_SEMI);
        if (head != NULL) return nd_block(head);
        return nd_block(NULL);
    }
    } /* end is_static scope */

    /* Label detection: identifier followed by colon */
    if (lex_tok == TK_IDENT) {
        /* Save lexer state for lookahead */
        memcpy(nm, lex_str, lex_slen + 1);
        sv_tok = lex_tok; sv_val = lex_val; sv_slen = lex_slen;
        sv_rcs = lex_rcs; sv_ract = lex_ract;
        sv_rp = lex_rp; sv_rts = lex_rts; sv_rte = lex_rte;
        memcpy(sv_str, lex_str, lex_slen + 1);
        next();
        if (lex_tok == TK_COLON) {
            /* It's a label */
            next();
            t = parse_stmt();
            return nd_label(find_or_add_label(nm), t);
        }
        /* Not a label — restore lexer state */
        lex_tok = sv_tok; lex_val = sv_val; lex_slen = sv_slen;
        lex_rcs = sv_rcs; lex_ract = sv_ract;
        lex_rp = sv_rp; lex_rts = sv_rts; lex_rte = sv_rte;
        memcpy(lex_str, sv_str, sv_slen + 1);
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

    expect(TK_LBRACE);
    saved_nlocals = ps_nlocals;
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
    ps_nlocals = saved_nlocals;
    return nd_block(head);
}

/* --- Top-level parser --- */

/* Parse type + name + optional pointer stars + params.
 * Handles: function definitions, function prototypes, global variables. */
static void parse_type_and_stars(int *out_ty) {
    *out_ty = parse_type();
}

static Node *parse_top_decl(void) {
    Node *fn;
    Node *phead;
    Node *ptail;
    Node *p;
    char nm[256];
    int ty;
    int pty;
    int off;
    int i;
    int count;
    int neg;
    int idx;
    int gi;
    int si;
    int base;
    int nf;
    int nsi;
    int nnf;
    int nj;
    int sp_idx;
    int slen;
    char *sp;

    /* Skip storage class / qualifier keywords (single-file compiler, no semantic effect) */
    while (lex_tok == TK_STATIC || lex_tok == TK_CONST ||
           lex_tok == TK_EXTERN || lex_tok == TK_INLINE ||
           lex_tok == TK_REGISTER || lex_tok == TK_RESTRICT ||
           lex_tok == TK_AUTO) next();

    /* Typedef */
    if (lex_tok == TK_TYPEDEF) {
        next();
        ty = parse_type();
        if (lex_tok == TK_LPAREN) {
            /* Function pointer typedef: typedef int (*Name)(args); */
            next();  /* skip ( */
            if (lex_tok == TK_STAR) next();  /* skip * */
            if (lex_tok != TK_IDENT) {
                p_error("expected typedef name");
                return NULL;
            }
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            expect(TK_RPAREN);
            /* Skip argument list */
            expect(TK_LPAREN);
            while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next();
            expect(TK_RPAREN);
            add_typedef(nm, TY_INT);  /* treat function pointers as int-sized */
            expect(TK_SEMI);
            return NULL;
        }
        if (lex_tok != TK_IDENT) {
            p_error("expected typedef name");
            return NULL;
        }
        add_typedef(lex_str, ty);
        next();
        expect(TK_SEMI);
        return NULL;
    }

    /* Enum definition at top level */
    if (lex_tok == TK_ENUM) {
        next();
        parse_enum_def();
        expect(TK_SEMI);
        return NULL;
    }

    /* Parse return type / variable type */
    ty = parse_type();

    /* Bare struct definition: struct Foo { ... }; */
    if (lex_tok == TK_SEMI && ty_is_struct(ty)) {
        next();
        return NULL;
    }

    /* Global function pointer: type (*name)(args); */
    if (lex_tok == TK_LPAREN) {
        next();
        if (lex_tok == TK_STAR) next();
        if (lex_tok != TK_IDENT) {
            p_error("expected name in fn ptr decl");
            return NULL;
        }
        memcpy(nm, lex_str, lex_slen + 1);
        next();
        expect(TK_RPAREN);
        if (lex_tok == TK_LPAREN) {
            next();
            while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next();
            expect(TK_RPAREN);
        }
        add_global(nm, TY_INT, 0);
        expect(TK_SEMI);
        return NULL;
    }

    /* Name */
    if (lex_tok != TK_IDENT) {
        p_error("expected name in declaration");
        return NULL;
    }
    memcpy(nm, lex_str, lex_slen + 1);
    next();

    /* Global variable: type name; or type name = expr; or type name[N]; */
    if (lex_tok == TK_SEMI || lex_tok == TK_ASSIGN) {
        if (ty_is_struct(ty)) {
            idx = add_global(nm, ty, ty_size(ty));
        } else {
            idx = add_global(nm, ty, 0);
        }
        if (lex_tok == TK_ASSIGN) {
            next();
            if (lex_tok == TK_LBRACE && ty_is_struct(ty)) {
                /* Struct initializer: struct S s = {v1, v2, ...}; */
                next();
                ps_ginit_start[idx] = ps_ginit_pool_len;
                si = ty_struct_idx(ty);
                base = st_first[si];
                nf = st_nfields[si];
                gi = 0;  /* counts total values consumed */
                i = 0;   /* field index */
                while (i < nf && lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                    if (ty_is_struct(stm_type[base + i])) {
                        /* Nested struct: flatten its fields */
                        nsi = ty_struct_idx(stm_type[base + i]);
                        nnf = st_nfields[nsi];
                        nj = 0;
                        while (nj < nnf && lex_tok != TK_RBRACE) {
                            if (gi > 0) expect(TK_COMMA);
                            if (ps_ginit_pool_len >= PS_MAX_INIT_POOL) {
                                p_error("init pool overflow"); break;
                            }
                            ps_ginit_pool[ps_ginit_pool_len] = parse_const_int();
                            ps_ginit_pool_len = ps_ginit_pool_len + 1;
                            gi = gi + 1;
                            nj = nj + 1;
                        }
                    } else {
                        if (gi > 0) expect(TK_COMMA);
                        if (ps_ginit_pool_len >= PS_MAX_INIT_POOL) {
                            p_error("init pool overflow"); break;
                        }
                        ps_ginit_pool[ps_ginit_pool_len] = parse_const_int();
                        ps_ginit_pool_len = ps_ginit_pool_len + 1;
                        gi = gi + 1;
                    }
                    i = i + 1;
                }
                ps_ginit_count[idx] = ps_ginit_pool_len - ps_ginit_start[idx];
                expect(TK_RBRACE);
            } else if (lex_tok == TK_STRING) {
                ps_gstr[idx] = lex_val;
                next();
            } else {
                neg = 0;
                if (lex_tok == TK_MINUS) { neg = 1; next(); }
                if (lex_tok == TK_NUM) {
                    ps_ginit[idx] = neg ? (0 - lex_val) : lex_val;
                    /* Sign-extend for long long globals */
                    if (ty_is_llong(ty)) {
                        if (ps_ginit[idx] < 0)
                            ps_ginit_hi[idx] = -1;
                        else
                            ps_ginit_hi[idx] = 0;
                    }
                    next();
                }
            }
        }
        expect(TK_SEMI);
        return NULL;
    }
    if (lex_tok == TK_LBRACK) {
        next();
        count = -1;
        if (lex_tok == TK_NUM) {
            count = lex_val;
            next();
        }
        expect(TK_RBRACK);
        if (lex_tok == TK_ASSIGN) {
            next();
            if (lex_tok == TK_STRING) {
                /* String array init: char s[N] = "str" or char s[] = "str" */
                sp_idx = lex_val;
                slen = lex_str_len[sp_idx];
                if (count < 0) count = slen + 1;
                idx = add_global(nm, ty + TY_PTR, ty_size(ty) * count);
                ps_ginit_start[idx] = ps_ginit_pool_len;
                sp = lex_strpool + lex_str_off[sp_idx];
                gi = 0;
                while (gi < slen && gi < count) {
                    ps_ginit_pool[ps_ginit_pool_len] = sp[gi] & 255;
                    ps_ginit_pool_len = ps_ginit_pool_len + 1;
                    gi = gi + 1;
                }
                if (gi < count) {
                    /* null terminator */
                    ps_ginit_pool[ps_ginit_pool_len] = 0;
                    ps_ginit_pool_len = ps_ginit_pool_len + 1;
                    gi = gi + 1;
                }
                ps_ginit_count[idx] = gi;
                next();
            } else if (lex_tok == TK_LBRACE) {
                next();
                si = ps_ginit_pool_len;  /* save pool start */
                gi = 0;
                while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                    if (gi > 0) expect(TK_COMMA);
                    if (count >= 0 && gi >= count) {
                        p_error("too many initializers"); break;
                    }
                    if (ps_ginit_pool_len >= PS_MAX_INIT_POOL) {
                        p_error("init pool overflow"); break;
                    }
                    ps_ginit_pool[ps_ginit_pool_len] = parse_const_int();
                    ps_ginit_pool_len = ps_ginit_pool_len + 1;
                    gi = gi + 1;
                }
                if (count < 0) count = gi;
                idx = add_global(nm, ty + TY_PTR, ty_size(ty) * count);
                ps_ginit_start[idx] = si;
                ps_ginit_count[idx] = gi;
                expect(TK_RBRACE);
            } else {
                p_error("expected string or { in array init");
                return NULL;
            }
        } else {
            if (count < 0) {
                p_error("array size required without initializer");
                return NULL;
            }
            idx = add_global(nm, ty + TY_PTR, ty_size(ty) * count);
        }
        expect(TK_SEMI);
        return NULL;
    }

    /* Function: type name(params) { body } or type name(params); */
    expect(TK_LPAREN);

    /* Reset locals for this function */
    i = 0;
    while (i < ps_nlocals) {
        free(ps_lname[i]);
        if (ps_lsname[i]) free(ps_lsname[i]);
        i = i + 1;
    }
    ps_nlocals = 0;
    ps_stack = 8;  /* reserve 8 bytes: saved r31 + saved r30 */
    ps_nparams = 0;
    ps_is_varargs = 0;
    ps_struct_ret = 0;
    ps_retptr_off = 0;
    ps_nlabels = 0;

    /* Hidden first param for struct return */
    if (ty_is_struct(ty)) {
        ps_struct_ret = 1;
        ps_retptr_off = add_local("__retptr", TY_PTR + TY_INT);
        ps_nparams = 1;
    }

    /* Parameters */
    phead = NULL;
    ptail = NULL;
    if (lex_tok != TK_RPAREN) {
        /* first param — or (void) meaning no params */
        if (!is_type()) {
            p_error("expected type in params");
            return NULL;
        }
        if (lex_tok == TK_VOID) {
            pty = parse_type();
            if (lex_tok == TK_RPAREN) {
                /* (void) → no params */
                goto params_done;
            }
        } else {
            pty = parse_type();
        }
        /* Function pointer param: type (*name)(args) */
        if (lex_tok == TK_LPAREN) {
            next();
            if (lex_tok == TK_STAR) next();
            if (lex_tok != TK_IDENT) { p_error("expected param name"); return NULL; }
            off = add_local(lex_str, TY_INT);
            p = nd_var(lex_str, off, TY_INT);
            p->is_local = 1;
            next();
            expect(TK_RPAREN);
            if (lex_tok == TK_LPAREN) {
                next();
                while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next();
                expect(TK_RPAREN);
            }
        } else if (lex_tok != TK_IDENT) {
            p_error("expected param name");
            return NULL;
        } else {
            off = add_local(lex_str, pty);
            p = nd_var(lex_str, off, pty);
            p->is_local = 1;
            next();
        }
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
            /* Function pointer param: type (*name)(args) */
            if (lex_tok == TK_LPAREN) {
                next();
                if (lex_tok == TK_STAR) next();
                if (lex_tok != TK_IDENT) { p_error("expected param name"); return NULL; }
                off = add_local(lex_str, TY_INT);
                p = nd_var(lex_str, off, TY_INT);
                p->is_local = 1;
                next();
                expect(TK_RPAREN);
                if (lex_tok == TK_LPAREN) {
                    next();
                    while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next();
                    expect(TK_RPAREN);
                }
            } else if (lex_tok != TK_IDENT) {
                p_error("expected param name");
                return NULL;
            } else {
                off = add_local(lex_str, pty);
                p = nd_var(lex_str, off, pty);
                p->is_local = 1;
                next();
            }
            ptail->next = p;
            ptail = p;
            ps_nparams = ps_nparams + 1;
        }
    }
params_done:
    expect(TK_RPAREN);

    /* Prototype: type name(params); */
    if (lex_tok == TK_SEMI) {
        next();
        add_func_type(nm, ty);
        return NULL;
    }

    /* Function body */
    add_func_type(nm, ty);
    fn = nd_new(ND_FUNC);
    fn->name = strdup(nm);
    fn->ty = ty;  /* store return type for sema pass */
    fn->args = phead;
    fn->nparams = ps_nparams;
    fn->is_varargs = ps_is_varargs;
    fn->offset = ps_struct_ret ? ps_retptr_off : 0; /* hidden __retptr offset */
    ps_cur_func = fn->name;
    fn->body = parse_block();
    fn->locals_size = ps_stack;

    return fn;
}

static Node *parse_program(void) {
    Node *prog;
    Node *fhead;
    Node *ftail;
    Node *f;

    ps_nglobals = 0;
    add_typedef("va_list", TY_PTR + TY_CHAR);
    next();  /* prime the first token */

    fhead = NULL;
    ftail = NULL;
    while (lex_tok != TK_EOF) {
        f = parse_top_decl();
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
