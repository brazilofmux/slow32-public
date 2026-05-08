/* parser.h -- Recursive-descent parser for stage04 compiler
 *
 * Phase 2: types, pointers, strings, globals, local arrays.
 * Builds a typed AST from the Ragel lexer token stream.
 * Compiled by stage03 s32-cc.
 */

/* Forward declarations (needed for host GCC compilation of cross-compiler) */
static int find_typedef(char *name);
static Node *parse_unary(void);
static int parse_string_literal(void);
static int parse_const_int(void);
static void next(void);

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

static int is_gnu_inline_ident(void) {
    if (lex_tok != TK_IDENT) return 0;
    if (strcmp(lex_str, "__inline") == 0) return 1;
    if (strcmp(lex_str, "__inline__") == 0) return 1;
    return 0;
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

static void skip_decl_qualifiers(void) {
    while (lex_tok == TK_CONST || lex_tok == TK_VOLATILE ||
           lex_tok == TK_RESTRICT || is_gnu_qual_ident() ||
           is_gnu_extension_ident()) next();
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
#define PS_MAX_CONSTS 2048

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
static int   ps_gextern[P_MAX_GLOBALS]; /* 1 = declaration only, no storage */
static int   ps_nglobals;

/* Static local variable state */
static char *ps_cur_func;             /* current function name, NULL outside */
static char  ps_sl_buf[256];          /* scratch buffer for name mangling */
static int   ps_sl_count;             /* global static-local counter */

/* Array/struct initializer bytes for globals */
#define PS_MAX_INIT_POOL 65536
static unsigned char ps_ginit_pool[PS_MAX_INIT_POOL];
static int ps_ginit_start[P_MAX_GLOBALS]; /* -1 = no init bytes */
static int ps_ginit_count[P_MAX_GLOBALS]; /* byte count */
static int ps_ginit_pool_len;

#define PS_MAX_INIT_RELOCS 8192
#define GIRELOC_STRING 0
#define GIRELOC_GLOBAL 1
#define GIRELOC_SYMBOL 2
static int ps_girel_start[P_MAX_GLOBALS];
static int ps_girel_count[P_MAX_GLOBALS];
static int ps_girel_off[PS_MAX_INIT_RELOCS];
static int ps_girel_kind[PS_MAX_INIT_RELOCS];
static int ps_girel_idx[PS_MAX_INIT_RELOCS];
static int ps_girel_size[PS_MAX_INIT_RELOCS];
static char *ps_girel_name[PS_MAX_INIT_RELOCS];
static int ps_ngirelocs;

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
#define PS_MAX_TYPEDEFS 1024
static char *ps_tdname[PS_MAX_TYPEDEFS];
static int   ps_tdtype[PS_MAX_TYPEDEFS];
static int   ps_ntypedefs;

/* Function return type table (for 64-bit return value tracking) */
#define PS_MAX_FUNCS 4096
static char *ps_fname[PS_MAX_FUNCS];
static int   ps_ftype[PS_MAX_FUNCS];
static int   ps_nfuncs;

/* Forward declarations */
static Node *parse_expr(void);
static Node *parse_stmt(void);
static Node *parse_assign(void);
static Node *parse_postfix(void);
static Node *parse_gnu_asm_stmt(void);

/* --- Utilities --- */

static void next(void) {
    int di;
    while (1) {
        if (pp_skip) {
            pp_fast_skip_to_directive();
            if (lex_pos >= lex_len) {
                lex_tok = TK_EOF;
                pp_sync();
                return;
            }
            pp_sync();
        }
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
    if (gnu_float_ident_ty() >= 0) return 1;
    if (is_gnu_extension_ident()) return 1;
    if (is_gnu_qual_ident()) return 1;
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
            val = parse_const_int();
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
    st_align[idx] = 1;
    st_is_union[idx] = 0;
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
        if (stm_owner[i] == si && strcmp(name, stm_name[i]) == 0) return i;
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
        if (stm_owner[i] == si) {
            if (seen == nth) return i;
            seen = seen + 1;
        }
        i = i + 1;
    }
    return -1;
}

/* Parse a type: int, char, void, struct, with optional pointer stars */
static int parse_type(void) {
    int ty;
    int si;
    int mty;
    int dty;
    int off;
    int max_sz;
    int arr_count;
    int val;
    int first_decl;
    char nm[256];
    /* Skip const/volatile/signed/restrict qualifiers */
    while (lex_tok == TK_CONST || lex_tok == TK_VOLATILE ||
           lex_tok == TK_SIGNED || lex_tok == TK_RESTRICT ||
           is_gnu_qual_ident() || is_gnu_extension_ident()) next();
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
        else if (lex_tok == TK_SHORT) {
            ty = TY_SHORT | TY_UNSIGNED;
            next();
            if (lex_tok == TK_INT) next();
        }
        else if (lex_tok == TK_INT) { ty = TY_INT | TY_UNSIGNED; next(); }
        else if (lex_tok == TK_LONG) {
            next();
            if (lex_tok == TK_LONG) {
                ty = TY_LLONG | TY_UNSIGNED; next();
            } else {
                ty = target_long_ty() | TY_UNSIGNED;
            }
            if (lex_tok == TK_INT) next();
        }
        else { ty = TY_INT | TY_UNSIGNED; }
    }
    else if (lex_tok == TK_LONG) {
        next();
        if (lex_tok == TK_LONG) { ty = TY_LLONG; next(); if (lex_tok == TK_INT) next(); }
        else if (lex_tok == TK_INT) { ty = target_long_ty(); next(); }
        else if (lex_tok == TK_UNSIGNED) {
            ty = target_long_ty() | TY_UNSIGNED;
            next();
            if (lex_tok == TK_INT) next();
        }
        else if (lex_tok == TK_SIGNED) {
            ty = target_long_ty();
            next();
            if (lex_tok == TK_INT) next();
        }
        else if (lex_tok == TK_DOUBLE) { ty = TY_DOUBLE; next(); }
        else { ty = target_long_ty(); }
    }
    else if (lex_tok == TK_FLOAT) { ty = TY_FLOAT; next(); }
    else if (lex_tok == TK_DOUBLE) { ty = TY_DOUBLE; next(); }
    else if (gnu_float_ident_ty() >= 0) {
        ty = gnu_float_ident_ty();
        next();
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
                    val = parse_const_int();
                }
                ps_cval[ps_nconsts] = val;
                ps_nconsts = ps_nconsts + 1;
                val = val + 1;
                if (lex_tok == TK_COMMA) next();
            }
            expect(TK_RBRACE);
        }
        ty = TY_INT;
    }
    else if (lex_tok == TK_STRUCT) {
        next();
        if (lex_tok == TK_IDENT) {
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            si = find_struct(nm);
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
            next();
            if (si < 0) {
                si = add_struct(nm);
            } else if (st_nfields[si] == 0 && st_size[si] == 0) {
                st_first[si] = stm_count;
            }
            off = 0;
            max_align = 1;
            while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                mty = parse_type();
                first_decl = 1;
                while (1) {
                    int is_fn_ptr_member;
                    int member_ty;

                    if (first_decl) {
                        dty = mty;
                        first_decl = 0;
                    } else {
                        dty = mty;
                        while (ty_is_ptr(dty)) dty = ty_deref(dty);
                        while (lex_tok == TK_STAR) { dty = dty + TY_PTR; next(); }
                    }
                    if (lex_tok == TK_COLON) {
                        int balign;
                        next();
                        arr_count = parse_const_int();
                        require_complete_type(dty, "incomplete bit-field type");
                        balign = ty_align(dty);
                        if (balign > 1) off = ((off + balign - 1) / balign) * balign;
                        if (balign > max_align) max_align = balign;
                        if (arr_count > 0) off = off + ty_size(dty);
                        break;
                    }
                    is_fn_ptr_member = 0;
                    member_ty = dty;
                    if (lex_tok == TK_LPAREN) {
                        next();
                        if (lex_tok == TK_STAR) next();
                        if (lex_tok != TK_IDENT) {
                            p_error("expected member name");
                            return TY_INT;
                        }
                        is_fn_ptr_member = 1;
                        member_ty = TY_PTR + TY_INT;
                    }
                    if (lex_tok != TK_IDENT) {
                        p_error("expected member name");
                        return TY_INT;
                    }
                    /* Align offset to the member's natural alignment.
                     * On 64-bit hosts (cc-x64, cc-a64) pointer/llong/double
                     * fields require 8-byte alignment so that JIT-emitted
                     * LDR X / STR X loads (scaled offsets) hit the right
                     * bytes; on SLOW-32 native ty_align() collapses to the
                     * old 1/2/4 behaviour. */
                    {
                        int malign = ty_align(member_ty);
                        if (malign > 1)
                            off = ((off + malign - 1) / malign) * malign;
                        if (malign > max_align) max_align = malign;
                    }
                    if (stm_count >= ST_MAX_MEMBERS) {
                        p_error("too many struct members");
                        return TY_INT;
                    }
                    stm_name[stm_count] = strdup(lex_str);
                    next();
                    if (is_fn_ptr_member) {
                        expect(TK_RPAREN);
                        if (lex_tok == TK_LPAREN) {
                            next();
                            while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next();
                            expect(TK_RPAREN);
                        }
                    }
                    /* Check for array member: type name[N][M]...; */
                    arr_count = 0;
                    while (lex_tok == TK_LBRACK) {
                        next();
                        if (lex_tok == TK_RBRACK) {
                            p_error("array size required in struct member");
                            return TY_INT;
                        }
                        if (arr_count == 0) arr_count = 1;
                        arr_count = arr_count * parse_const_int();
                        expect(TK_RBRACK);
                    }
                    skip_gnu_decl_suffixes();
                    if (arr_count > 0) {
                        require_complete_type(member_ty, "incomplete struct member");
                        stm_owner[stm_count] = si;
                        stm_type[stm_count] = member_ty + TY_PTR;
                        stm_is_arr[stm_count] = 1;
                        stm_arr_size[stm_count] = ty_size(member_ty) * arr_count;
                        stm_off[stm_count] = off;
                        off = off + ty_size(member_ty) * arr_count;
                    } else {
                        require_complete_type(member_ty, "incomplete struct member");
                        stm_owner[stm_count] = si;
                        stm_type[stm_count] = member_ty;
                        stm_is_arr[stm_count] = 0;
                        stm_arr_size[stm_count] = 0;
                        stm_off[stm_count] = off;
                        off = off + ty_size(member_ty);
                    }
                    stm_count = stm_count + 1;
                    st_nfields[si] = st_nfields[si] + 1;
                    if (lex_tok != TK_COMMA) break;
                    next();
                }
                expect(TK_SEMI);
            }
            expect(TK_RBRACE);
            /* Round total size up to the struct's alignment so that arrays
             * of struct keep each element naturally aligned.  The historical
             * "round to 4" rule is a special case of this when max_align <= 4. */
            st_align[si] = max_align;
            if (max_align < 4) max_align = 4;
            st_size[si] = ((off + max_align - 1) / max_align) * max_align;
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
            si = find_struct(nm);
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
            if (si < 0) {
                si = add_struct(nm);
            } else if (st_nfields[si] == 0 && st_size[si] == 0) {
                st_first[si] = stm_count;
            }
            st_is_union[si] = 1;
            max_sz = 0;
            max_align = 1;
            while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                mty = parse_type();
                first_decl = 1;
                while (1) {
                    int is_fn_ptr_member;
                    int member_ty;

                    if (first_decl) {
                        dty = mty;
                        first_decl = 0;
                    } else {
                        dty = mty;
                        while (ty_is_ptr(dty)) dty = ty_deref(dty);
                        while (lex_tok == TK_STAR) { dty = dty + TY_PTR; next(); }
                    }
                    if (lex_tok == TK_COLON) {
                        next();
                        arr_count = parse_const_int();
                        require_complete_type(dty, "incomplete bit-field type");
                        if (arr_count > 0 && ty_size(dty) > max_sz) max_sz = ty_size(dty);
                        break;
                    }
                    is_fn_ptr_member = 0;
                    member_ty = dty;
                    if (lex_tok == TK_LPAREN) {
                        next();
                        if (lex_tok == TK_STAR) next();
                        if (lex_tok != TK_IDENT) {
                            p_error("expected member name");
                            return TY_INT;
                        }
                        is_fn_ptr_member = 1;
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
                    next();
                    if (is_fn_ptr_member) {
                        expect(TK_RPAREN);
                        if (lex_tok == TK_LPAREN) {
                            next();
                            while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next();
                            expect(TK_RPAREN);
                        }
                    }
                    /* Check for array member: type name[N][M]...; */
                    arr_count = 0;
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
                        stm_is_arr[stm_count] = 1;
                        stm_arr_size[stm_count] = ty_size(member_ty) * arr_count;
                        stm_off[stm_count] = 0;
                        if (ty_size(member_ty) * arr_count > max_sz) max_sz = ty_size(member_ty) * arr_count;
                        malign = ty_align(member_ty);
                        if (malign > max_align) max_align = malign;
                    } else {
                        int malign;
                        require_complete_type(member_ty, "incomplete union member");
                        stm_owner[stm_count] = si;
                        stm_type[stm_count] = member_ty;
                        stm_is_arr[stm_count] = 0;
                        stm_arr_size[stm_count] = 0;
                        stm_off[stm_count] = 0;
                        if (ty_size(member_ty) > max_sz) max_sz = ty_size(member_ty);
                        malign = ty_align(member_ty);
                        if (malign > max_align) max_align = malign;
                    }
                    stm_count = stm_count + 1;
                    st_nfields[si] = st_nfields[si] + 1;
                    if (lex_tok != TK_COMMA) break;
                    next();
                }
                expect(TK_SEMI);
            }
            expect(TK_RBRACE);
            /* Round total size up to the union's alignment so that arrays
             * of union keep each element naturally aligned. */
            st_align[si] = max_align;
            if (max_align < 4) max_align = 4;
            st_size[si] = ((max_sz + max_align - 1) / max_align) * max_align;
        } else {
            /* Forward declaration or reference: union Name (no brace) */
            if (si < 0) {
                si = add_struct(nm);
                st_is_union[si] = 1;
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
    require_complete_type(ty, "incomplete type for local");
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
    require_complete_type(elem_ty, "incomplete element type");
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
    ps_ginit[idx] = 0;
    ps_ginit_hi[idx] = 0;
    ps_gstr[idx] = -1;
    ps_ginit_start[idx] = -1;
    ps_ginit_count[idx] = 0;
    ps_girel_start[idx] = -1;
    ps_girel_count[idx] = 0;
    ps_glocal[idx] = 0;
    ps_gextern[idx] = 0;
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
}

static int add_extern_global(char *name, int ty, int size_bytes) {
    int idx;
    idx = find_global(name);
    if (idx >= 0) return idx;
    idx = add_global(name, ty, size_bytes);
    ps_gextern[idx] = 1;
    return idx;
}

static int add_defined_global(char *name, int ty, int size_bytes) {
    int idx;
    idx = find_global(name);
    if (idx >= 0 && ps_gextern[idx]) {
        ps_gtype[idx] = ty;
        ps_gsize[idx] = size_bytes;
        ps_glocal[idx] = 0;
        ps_gextern[idx] = 0;
        ps_reset_global_init(idx);
        return idx;
    }
    return add_global(name, ty, size_bytes);
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

static int parse_const_primary(void) {
    int ci;
    int ty;

    if (lex_tok == TK_LPAREN) {
        next();
        ci = parse_const_int();
        expect(TK_RPAREN);
        return ci;
    }
    if (lex_tok == TK_SIZEOF) {
        next();
        expect(TK_LPAREN);
        if (!is_type()) p_error("expected type in sizeof");
        ty = parse_type();
        while (lex_tok == TK_STAR) { ty = ty + TY_PTR; next(); }
        skip_decl_qualifiers();
        expect(TK_RPAREN);
        return ty_size(ty);
    }
    if (lex_tok == TK_NUM) {
        ci = lex_val;
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

static int parse_const_unary(void) {
    int ty;
    int sv_tok;
    int sv_val;
    int sv_slen;
    int sv_rcs;
    int sv_ract;
    char *sv_rp;
    char *sv_rts;
    char *sv_rte;
    char sv_str[256];

    if (lex_tok == TK_PLUS) {
        next();
        return parse_const_unary();
    }
    if (lex_tok == TK_MINUS) {
        next();
        return 0 - parse_const_unary();
    }
    if (lex_tok == TK_LPAREN) {
        sv_tok = lex_tok; sv_val = lex_val; sv_slen = lex_slen;
        sv_rcs = lex_rcs; sv_ract = lex_ract;
        sv_rp = lex_rp; sv_rts = lex_rts; sv_rte = lex_rte;
        memcpy(sv_str, lex_str, lex_slen + 1);
        next();
        if (is_type()) {
            ty = parse_type();
            while (lex_tok == TK_STAR) { ty = ty + TY_PTR; next(); }
            skip_decl_qualifiers();
            expect(TK_RPAREN);
            return parse_const_unary();
        }
        lex_tok = sv_tok; lex_val = sv_val; lex_slen = sv_slen;
        lex_rcs = sv_rcs; lex_ract = sv_ract;
        lex_rp = sv_rp; lex_rts = sv_rts; lex_rte = sv_rte;
        memcpy(lex_str, sv_str, sv_slen + 1);
    }
    return parse_const_primary();
}

static int parse_const_mul(void) {
    int v;
    int r;

    v = parse_const_unary();
    while (lex_tok == TK_STAR || lex_tok == TK_SLASH || lex_tok == TK_PERCENT) {
        if (lex_tok == TK_STAR) {
            next();
            v = v * parse_const_unary();
        } else if (lex_tok == TK_SLASH) {
            next();
            r = parse_const_unary();
            if (r == 0) p_error("division by zero in constant expression");
            v = v / r;
        } else {
            next();
            r = parse_const_unary();
            if (r == 0) p_error("division by zero in constant expression");
            v = v % r;
        }
    }
    return v;
}

/* Parse a compile-time constant integer (for initializers and array sizes) */
static int parse_const_int(void) {
    int v;

    v = parse_const_mul();
    while (lex_tok == TK_PLUS || lex_tok == TK_MINUS) {
        if (lex_tok == TK_PLUS) {
            next();
            v = v + parse_const_mul();
        } else {
            next();
            v = v - parse_const_mul();
        }
    }
    return v;
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

static void ps_ginit_emit_zeroes(int n) {
    int i;
    i = 0;
    while (i < n) {
        ps_ginit_emit_byte(0);
        i = i + 1;
    }
}

static void ps_ginit_emit_int(int v, int sz) {
    int i;
    i = 0;
    while (i < sz) {
        ps_ginit_emit_byte((v >> (i * 8)) & 255);
        i = i + 1;
    }
}

static void ps_ginit_begin(int gidx) {
    ps_ginit_start[gidx] = ps_ginit_pool_len;
    ps_ginit_count[gidx] = 0;
    ps_girel_start[gidx] = ps_ngirelocs;
    ps_girel_count[gidx] = 0;
}

static void ps_ginit_finish(int gidx) {
    ps_ginit_count[gidx] = ps_ginit_pool_len - ps_ginit_start[gidx];
    ps_girel_count[gidx] = ps_ngirelocs - ps_girel_start[gidx];
}

static void ps_ginit_add_reloc(int gidx, int kind, int idx, int sz) {
    if (ps_ngirelocs >= PS_MAX_INIT_RELOCS) p_error("too many init relocs");
    ps_girel_off[ps_ngirelocs] = ps_ginit_cur_off(gidx);
    ps_girel_kind[ps_ngirelocs] = kind;
    ps_girel_idx[ps_ngirelocs] = idx;
    ps_girel_size[ps_ngirelocs] = sz;
    ps_girel_name[ps_ngirelocs] = NULL;
    ps_ngirelocs = ps_ngirelocs + 1;
    ps_ginit_emit_zeroes(sz);
}

static void ps_ginit_add_sym_reloc(int gidx, char *name, int sz) {
    if (ps_ngirelocs >= PS_MAX_INIT_RELOCS) p_error("too many init relocs");
    ps_girel_off[ps_ngirelocs] = ps_ginit_cur_off(gidx);
    ps_girel_kind[ps_ngirelocs] = GIRELOC_SYMBOL;
    ps_girel_idx[ps_ngirelocs] = 0;
    ps_girel_size[ps_ngirelocs] = sz;
    ps_girel_name[ps_ngirelocs] = strdup(name);
    ps_ngirelocs = ps_ngirelocs + 1;
    ps_ginit_emit_zeroes(sz);
}

static int try_consume_type_cast(void) {
    int sv_tok;
    int sv_val;
    int sv_slen;
    int sv_rcs;
    int sv_ract;
    char *sv_rp;
    char *sv_rts;
    char *sv_rte;
    char sv_str[256];
    int ty;

    if (lex_tok != TK_LPAREN) return 0;
    sv_tok = lex_tok; sv_val = lex_val; sv_slen = lex_slen;
    sv_rcs = lex_rcs; sv_ract = lex_ract;
    sv_rp = lex_rp; sv_rts = lex_rts; sv_rte = lex_rte;
    memcpy(sv_str, lex_str, lex_slen + 1);

    next();
    if (!is_type()) {
        lex_tok = sv_tok; lex_val = sv_val; lex_slen = sv_slen;
        lex_rcs = sv_rcs; lex_ract = sv_ract;
        lex_rp = sv_rp; lex_rts = sv_rts; lex_rte = sv_rte;
        memcpy(lex_str, sv_str, sv_slen + 1);
        return 0;
    }
    ty = parse_type();
    (void)ty;
    while (lex_tok == TK_STAR) next();
    skip_decl_qualifiers();
    expect(TK_RPAREN);
    return 1;
}

static int parse_global_init_symbol_reloc(int gidx, int sz) {
    char nm[256];
    int ci;

    while (try_consume_type_cast()) {
    }
    if (lex_tok != TK_IDENT) return 0;
    ci = find_const(lex_str);
    if (ci >= 0) return 0;
    memcpy(nm, lex_str, lex_slen + 1);
    next();
    ps_ginit_add_sym_reloc(gidx, nm, sz);
    return 1;
}

static void parse_global_init_value(int ty, int gidx);

static void parse_global_init_array_fixed(int elem_ty, int count, int gidx) {
    int i;
    int elem_sz;
    int sp_idx;
    int slen;
    char *sp;
    int start;

    elem_sz = ty_size(elem_ty);
    start = ps_ginit_cur_off(gidx);
    if ((elem_ty & TY_BASE_MASK) == TY_CHAR && lex_tok == TK_STRING) {
        sp_idx = parse_string_literal();
        slen = lex_str_len[sp_idx];
        sp = lex_strpool + lex_str_off[sp_idx];
        i = 0;
        while (i < slen && i < count) {
            ps_ginit_emit_byte(sp[i] & 255);
            i = i + 1;
        }
        if (i < count) {
            ps_ginit_emit_byte(0);
            i = i + 1;
        }
        while (i < count) {
            ps_ginit_emit_zeroes(elem_sz);
            i = i + 1;
        }
        return;
    }
    if (lex_tok != TK_LBRACE) {
        p_error("expected { in aggregate initializer");
        return;
    }
    next();
    i = 0;
    while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
        if (i > 0) {
            expect(TK_COMMA);
            if (lex_tok == TK_RBRACE) break;
        }
        if (i >= count) p_error("too many initializers");
        parse_global_init_value(elem_ty, gidx);
        i = i + 1;
    }
    if (lex_tok == TK_COMMA) next();
    expect(TK_RBRACE);
    while (ps_ginit_cur_off(gidx) < start + (count * elem_sz)) {
        ps_ginit_emit_byte(0);
    }
}

static void parse_global_init_struct(int ty, int gidx) {
    int si;
    int nf;
    int i;
    int mi;
    int field_start;
    int field_ty;
    int arr_elem_ty;
    int arr_count;
    int struct_start;
    int has_brace;
    char dnm[256];

    si = ty_struct_idx(ty);
    nf = st_nfields[si];
    struct_start = ps_ginit_cur_off(gidx);
    /* Brace is optional for nested struct fields inside a flat-form parent
     * initializer (e.g. `struct Rect gr = {10, 20, 30, 40}`).  When called
     * at top-level the caller has already verified TK_LBRACE is present. */
    has_brace = (lex_tok == TK_LBRACE);
    if (has_brace) next();
    i = 0;
    while (lex_tok != TK_EOF && i < nf) {
        if (has_brace && lex_tok == TK_RBRACE) break;
        if (i > 0) {
            if (has_brace) {
                expect(TK_COMMA);
                if (lex_tok == TK_RBRACE) break;
            } else {
                /* No outer brace: the parent's loop will consume the trailing
                 * comma after we return.  Stop if there isn't one. */
                if (lex_tok != TK_COMMA) break;
                expect(TK_COMMA);
            }
        }
        if (lex_tok == TK_DOT) {
            next();
            if (lex_tok != TK_IDENT) p_error("expected field name in initializer");
            memcpy(dnm, lex_str, lex_slen + 1);
            next();
            expect(TK_ASSIGN);
            mi = find_member(ty, dnm);
        } else {
            mi = struct_field_nth_idx(si, i);
        }
        if (mi < 0) p_error("missing struct field");
        field_start = struct_start + stm_off[mi];
        while (ps_ginit_cur_off(gidx) < field_start) ps_ginit_emit_byte(0);
        field_ty = stm_type[mi];
        if (stm_is_arr[mi]) {
            arr_elem_ty = ty_deref(field_ty);
            arr_count = stm_arr_size[mi] / ty_size(arr_elem_ty);
            parse_global_init_array_fixed(arr_elem_ty, arr_count, gidx);
        } else {
            parse_global_init_value(field_ty, gidx);
        }
        i = i + 1;
    }
    if (has_brace) {
        if (lex_tok == TK_COMMA) next();
        expect(TK_RBRACE);
    }
    while (ps_ginit_cur_off(gidx) < struct_start + ty_size(ty)) {
        ps_ginit_emit_byte(0);
    }
}

static void parse_global_init_value(int ty, int gidx) {
    int v;
    int sp_idx;

    if (ty_is_struct(ty)) {
        parse_global_init_struct(ty, gidx);
        return;
    }
    if (ty_is_ptr(ty) && lex_tok == TK_STRING) {
        sp_idx = parse_string_literal();
        ps_ginit_add_reloc(gidx, GIRELOC_STRING, sp_idx, ty_size(ty));
        return;
    }
    if (ty_is_ptr(ty) && parse_global_init_symbol_reloc(gidx, ty_size(ty))) {
        return;
    }
    v = parse_const_int();
    ps_ginit_emit_int(v, ty_size(ty));
}

static int parse_string_literal(void) {
    int idxs[64];
    int nidx;
    int total;
    int start;
    int i;
    int j;
    int src;

    nidx = 0;
    total = 0;
    while (lex_tok == TK_STRING) {
        if (nidx >= 64) p_error("too many adjacent string literals");
        idxs[nidx] = lex_val;
        total = total + lex_str_len[lex_val];
        nidx = nidx + 1;
        next();
    }
    if (nidx == 1) return idxs[0];
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
    lex_str_off[lex_str_count] = start;
    lex_str_len[lex_str_count] = total;
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
        v = parse_string_literal();
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
                n->is_array = ps_larr[li];
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

        /* __builtin_expect(expr, val) — returns expr (hint ignored) */
        if (strcmp(nm, "__builtin_expect") == 0) {
            expect(TK_LPAREN);
            n = parse_assign();
            expect(TK_COMMA);
            parse_assign();  /* discard second arg (the expected value) */
            expect(TK_RPAREN);
            return n;
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

    /* __builtin_offsetof(type, member) / offsetof(type, member) */
    if (lex_tok == TK_OFFSETOF) {
        int oty;
        int omi;
        char omn[256];
        next();
        expect(TK_LPAREN);
        oty = parse_type();
        if (!ty_is_struct(oty)) {
            p_error("offsetof requires struct/union type");
            return nd_num(0);
        }
        expect(TK_COMMA);
        if (lex_tok != TK_IDENT) {
            p_error("expected member name in offsetof");
            return nd_num(0);
        }
        memcpy(omn, lex_str, lex_slen + 1);
        next();
        omi = find_member(oty, omn);
        if (omi < 0) {
            p_error("unknown member in offsetof");
            return nd_num(0);
        }
        v = stm_off[omi];
        expect(TK_RPAREN);
        return nd_num(v);
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
    int count2;
    int ci;
    int cv;
    int neg;
    int si;
    int base;
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
    /* Lexer save/restore for label lookahead */
    int sv_tok; int sv_val; int sv_slen; int sv_rcs; int sv_ract;
    char *sv_rp; char *sv_rts; char *sv_rte;
    char sv_str[256];

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
        sa_val = (sa_expr && sa_expr->kind == ND_NUM) ? sa_expr->val : 1;
        expect(TK_COMMA);
        if (lex_tok == TK_STRING) next();
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
            skip_decl_qualifiers();
            while (lex_tok == TK_STAR) {
                ty = ty + TY_PTR;
                next();
                skip_decl_qualifiers();
            }
            if (lex_tok != TK_IDENT) {
                p_error("expected identifier in for-init");
                return nd_num(0);
            }
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            off = add_local(nm, ty);
            if (lex_tok == TK_ASSIGN) {
                next();
                n = nd_assign(nd_var(nm, off, ty), parse_assign());
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
        if (lex_tok == TK_NUM || lex_tok == TK_CHARLIT) {
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
    int is_extern;
    int sl_gi;
    int sl_li;
    is_static = 0;
    is_extern = 0;
    skip_gnu_attributes();
    while (lex_tok == TK_STATIC || lex_tok == TK_CONST ||
           lex_tok == TK_EXTERN ||
           lex_tok == TK_REGISTER || lex_tok == TK_RESTRICT ||
           lex_tok == TK_AUTO || is_gnu_qual_ident() ||
           is_gnu_extension_ident() || is_gnu_inline_ident() ||
           is_gnu_attr_ident()) {
        if (lex_tok == TK_STATIC) is_static = 1;
        if (lex_tok == TK_EXTERN) is_extern = 1;
        if (is_gnu_attr_ident()) skip_gnu_attributes();
        else next();
    }

    /* local variable declaration */
    if (is_type()) {
        ty = parse_type();
        skip_decl_qualifiers();
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
                n = nd_assign(nd_var(nm, off, TY_INT), parse_assign());
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
        skip_gnu_decl_suffixes();

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
            }
            expect(TK_SEMI);
            return nd_block(NULL);
        }

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
            if (lex_tok != TK_RBRACK) {
                count = parse_const_int();
            }
            expect(TK_RBRACK);
            while (lex_tok == TK_LBRACK) {
                next();
                count2 = -1;
                if (lex_tok != TK_RBRACK) {
                    count2 = parse_const_int();
                }
                expect(TK_RBRACK);
                if (count < 0 || count2 < 0) {
                    p_error("array size required without initializer");
                    return nd_num(0);
                }
                count = count * count2;
            }
            if (is_static) {
                sp_idx = -1;
                neg = 0;
                if (count < 0) {
                    if (lex_tok == TK_ASSIGN) {
                        next();
                        if (lex_tok == TK_STRING) {
                            sp_idx = parse_string_literal();
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
                    if (lex_tok == TK_STRING) {
                        sp_idx = parse_string_literal();
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
                    ci = 0;
                    expect(TK_LBRACE);
                    while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                        if (ci > 0) {
                            expect(TK_COMMA);
                            if (lex_tok == TK_RBRACE) break;
                        }
                        if (count >= 0 && ci >= count) p_error("too many initializers");
                        parse_global_init_value(ty, sl_gi);
                        ci = ci + 1;
                    }
                    if (lex_tok == TK_COMMA) next();
                    expect(TK_RBRACE);
                    if (count < 0) count = ci;
                    ps_gsize[sl_gi] = ty_size(ty) * count;
                    while (ps_ginit_cur_off(sl_gi) < ps_gsize[sl_gi]) {
                        ps_ginit_emit_byte(0);
                    }
                    ps_ginit_finish(sl_gi);
                }
                expect(TK_SEMI);
                sl_li = ps_nlocals;
                if (sl_li >= P_MAX_LOCALS) { p_error("too many locals"); return nd_num(0); }
                ps_lname[sl_li] = strdup(nm);
                ps_loff[sl_li] = 0;
                ps_ltype[sl_li] = ty + TY_PTR;
                ps_larr[sl_li] = 1;
                ps_lstatic[sl_li] = 1;
                ps_lsname[sl_li] = strdup(ps_sl_buf);
                ps_nlocals = ps_nlocals + 1;
                return nd_block(NULL);
            }
            head = NULL;
            if (lex_tok == TK_ASSIGN) {
                next();
                if (lex_tok == TK_STRING) {
                    /* String array init: char s[N] = "str" or char s[] = "str" */
                    sp_idx = parse_string_literal();
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
                nf = st_nfields[si];
                ci = 0;  /* field index */
                while (ci < nf && lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                    if (ci > 0) {
                        expect(TK_COMMA);
                        if (lex_tok == TK_RBRACE) break;
                    }
                    if (lex_tok == TK_DOT) {
                        next();
                        if (lex_tok != TK_IDENT) p_error("expected field name in initializer");
                        memcpy(dnm, lex_str, lex_slen + 1);
                        next();
                        expect(TK_ASSIGN);
                        mi = find_member(ty, dnm);
                    } else {
                        mi = struct_field_nth_idx(si, ci);
                    }
                    if (mi < 0) p_error("missing struct field");
                    if (ty_is_struct(stm_type[mi])) {
                        /* Nested struct: flatten */
                        neg = 0;
                        nsi = ty_struct_idx(stm_type[mi]);
                        nnf = st_nfields[nsi];
                        nj = 0;
                        if (lex_tok == TK_LBRACE) {
                            neg = 1;
                            next();
                        }
                        while (nj < nnf && lex_tok != TK_RBRACE) {
                            if (nj > 0) expect(TK_COMMA);
                            base = struct_field_nth_idx(nsi, nj);
                            if (base < 0) p_error("missing nested struct field");
                            mi_off = stm_off[mi] + stm_off[base];
                            mi_ty = stm_type[base];
                            n = nd_var(nm, off, ty);
                            n->is_local = 1;
                            a = nd_member(n, mi_off, mi_ty, 0, 0);
                            a = nd_assign(a, parse_assign());
                            a = nd_expr_stmt(a);
                            if (head == NULL) { head = a; tail = a; }
                            else { tail->next = a; tail = a; }
                            nj = nj + 1;
                        }
                        if (neg) expect(TK_RBRACE);
                    } else {
                        mi_off = stm_off[mi];
                        mi_ty = stm_type[mi];
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
                if (lex_tok == TK_COMMA) next();
                expect(TK_RBRACE);
                expect(TK_SEMI);
                if (head != NULL) return nd_block(head);
                return nd_block(NULL);
            }
            n = nd_assign(nd_var(nm, off, ty), parse_assign());
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
            skip_decl_qualifiers();
            if (lex_tok != TK_IDENT) break;
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            off = add_local(nm, xty);
            if (lex_tok == TK_ASSIGN) {
                next();
                n = nd_assign(nd_var(nm, off, xty), parse_assign());
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
    char pnm[256];
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
    int saw_unnamed_param;
    int xty;
    int is_extern;
    int is_static;
    int decl_had_init;
    char *sp;

    /* _Static_assert(expr, "message") — compile-time check, no codegen */
    if (lex_tok == TK_STATIC_ASSERT) {
        int sa_val;
        Node *sa_expr;
        next();
        expect(TK_LPAREN);
        sa_expr = parse_assign();
        /* Evaluate constant expression — only supports ND_NUM for now */
        sa_val = (sa_expr && sa_expr->kind == ND_NUM) ? sa_expr->val : 1;
        expect(TK_COMMA);
        if (lex_tok == TK_STRING) next(); /* skip message string */
        expect(TK_RPAREN);
        expect(TK_SEMI);
        if (sa_val == 0) p_error("_Static_assert failed");
        return NULL;  /* no AST node emitted */
    }

    /* Storage class / qualifier keywords.  We honour `static` on
     * function definitions (emit STB_LOCAL); other qualifiers (const,
     * inline, register, restrict, auto, GNU attrs) have no semantic
     * effect in this single-file compiler. */
    is_extern = 0;
    is_static = 0;
    skip_gnu_attributes();
    while (lex_tok == TK_STATIC || lex_tok == TK_CONST ||
           lex_tok == TK_EXTERN || lex_tok == TK_INLINE ||
           lex_tok == TK_REGISTER || lex_tok == TK_RESTRICT ||
           lex_tok == TK_AUTO || is_gnu_qual_ident() ||
           is_gnu_extension_ident() || is_gnu_inline_ident() ||
           is_gnu_attr_ident()) {
        if (lex_tok == TK_EXTERN) is_extern = 1;
        if (lex_tok == TK_STATIC) is_static = 1;
        if (is_gnu_attr_ident()) skip_gnu_attributes();
        else next();
    }

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
        memcpy(nm, lex_str, lex_slen + 1);
        next();
        if (lex_tok == TK_LPAREN) {
            next();
            while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next();
            expect(TK_RPAREN);
            add_typedef(nm, TY_INT);  /* function types decay to pointer use-sites here */
            skip_gnu_decl_suffixes();
            expect(TK_SEMI);
            return NULL;
        }
        if (lex_tok == TK_LBRACK) {
            next();
            if (lex_tok != TK_RBRACK) parse_const_int();
            expect(TK_RBRACK);
            add_typedef(nm, ty + TY_PTR);  /* array typedefs decay to pointer use-sites here */
            skip_gnu_decl_suffixes();
            expect(TK_SEMI);
            return NULL;
        }
        add_typedef(nm, ty);
        skip_gnu_decl_suffixes();
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
    skip_decl_qualifiers();

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
        skip_gnu_decl_suffixes();
        if (is_extern) idx = add_extern_global(nm, TY_INT, 0);
        else           idx = add_defined_global(nm, TY_INT, 0);
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
    skip_gnu_decl_suffixes();

    /* Global scalar(s): type name; type name = expr; or type a, b = expr; */
    if (lex_tok == TK_SEMI || lex_tok == TK_ASSIGN || lex_tok == TK_COMMA) {
        xty = ty;
        while (1) {
            decl_had_init = (lex_tok == TK_ASSIGN);
            if (ty_is_struct(xty)) {
                require_complete_type(xty, "incomplete global type");
                if (is_extern && !decl_had_init)
                    idx = add_extern_global(nm, xty, ty_size(xty));
                else
                    idx = add_defined_global(nm, xty, ty_size(xty));
            } else {
                if (is_extern && !decl_had_init)
                    idx = add_extern_global(nm, xty, 0);
                else
                    idx = add_defined_global(nm, xty, 0);
            }
            skip_gnu_decl_suffixes();
            if (lex_tok == TK_ASSIGN) {
                next();
                if (lex_tok == TK_LBRACE && ty_is_struct(xty)) {
                    /* Struct initializer: struct S s = { ... }; */
                    ps_ginit_begin(idx);
                    parse_global_init_value(xty, idx);
                    ps_ginit_finish(idx);
                } else if (lex_tok == TK_STRING) {
                    ps_gstr[idx] = parse_string_literal();
                } else {
                    ps_ginit[idx] = parse_const_int();
                    /* Sign-extend for long long globals */
                    if (ty_is_llong(xty)) {
                        if (ps_ginit[idx] < 0)
                            ps_ginit_hi[idx] = -1;
                        else
                            ps_ginit_hi[idx] = 0;
                    }
                }
            }
            if (lex_tok != TK_COMMA) break;
            next();
            xty = ty;
            while (ty_is_ptr(xty)) xty = ty_deref(xty);
            while (lex_tok == TK_STAR) { xty = xty + TY_PTR; next(); }
            skip_decl_qualifiers();
            if (lex_tok != TK_IDENT) {
                p_error("expected name in declaration");
                return NULL;
            }
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            skip_gnu_decl_suffixes();
        }
        expect(TK_SEMI);
        return NULL;
    }
    if (lex_tok == TK_LBRACK) {
        next();
        count = -1;
        if (lex_tok != TK_RBRACK) {
            count = parse_const_int();
        }
        expect(TK_RBRACK);
        skip_gnu_decl_suffixes();
        if (lex_tok == TK_ASSIGN) {
            next();
            if (lex_tok == TK_STRING) {
                /* String array init: char s[N] = "str" or char s[] = "str"
                 * (with adjacent-string concatenation via parse_string_literal). */
                sp_idx = parse_string_literal();
                slen = lex_str_len[sp_idx];
                if (count < 0) count = slen + 1;
                require_complete_type(ty, "incomplete element type");
                idx = add_defined_global(nm, ty + TY_PTR, ty_size(ty) * count);
                ps_ginit_begin(idx);
                /* parse_string_literal already consumed the literal, so emit
                 * its bytes directly rather than calling
                 * parse_global_init_array_fixed (which would try to re-parse
                 * a string that's no longer at the lexer cursor). */
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
                gi = 0;
                idx = add_defined_global(nm, ty + TY_PTR, 0);
                ps_ginit_begin(idx);
                expect(TK_LBRACE);
                while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                    if (gi > 0) {
                        expect(TK_COMMA);
                        if (lex_tok == TK_RBRACE) break;
                    }
                    if (count >= 0 && gi >= count) p_error("too many initializers");
                    parse_global_init_value(ty, idx);
                    gi = gi + 1;
                }
                if (lex_tok == TK_COMMA) next();
                expect(TK_RBRACE);
                if (count < 0) count = gi;
                require_complete_type(ty, "incomplete element type");
                ps_gsize[idx] = ty_size(ty) * count;
                while (ps_ginit_cur_off(idx) < ps_gsize[idx]) ps_ginit_emit_byte(0);
                ps_ginit_finish(idx);
            } else {
                p_error("expected string or { in array init");
                return NULL;
            }
        } else {
            if (count < 0) {
                if (!is_extern) {
                    p_error("array size required without initializer");
                    return NULL;
                }
                count = 1;
            }
            require_complete_type(ty, "incomplete element type");
            if (is_extern) idx = add_extern_global(nm, ty + TY_PTR, ty_size(ty) * count);
            else           idx = add_defined_global(nm, ty + TY_PTR, ty_size(ty) * count);
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
    if (lex_tok != TK_RPAREN) {
        /* first param — or (void) meaning no params */
        if (!is_type()) {
            p_error("expected type in params");
            return NULL;
        }
        if (lex_tok == TK_VOID) {
            pty = parse_type();
            skip_decl_qualifiers();
            if (lex_tok == TK_RPAREN) {
                /* (void) → no params */
                goto params_done;
            }
        } else {
            pty = parse_type();
            skip_decl_qualifiers();
        }
        while (lex_tok == TK_STAR) {
            pty = pty + TY_PTR;
            next();
            skip_decl_qualifiers();
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
        } else if (lex_tok == TK_LBRACK) {
            while (lex_tok == TK_LBRACK) {
                pty = pty + TY_PTR;
                next();
                if (lex_tok != TK_RBRACK) parse_const_int();
                expect(TK_RBRACK);
            }
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
            while (lex_tok == TK_LBRACK) {
                pty = pty + TY_PTR;
                next();
                if (lex_tok != TK_RBRACK) parse_const_int();
                expect(TK_RBRACK);
            }
            off = add_local(pnm, pty);
            p = nd_var(pnm, off, pty);
            p->is_local = 1;
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
            skip_decl_qualifiers();
            while (lex_tok == TK_STAR) {
                pty = pty + TY_PTR;
                next();
                skip_decl_qualifiers();
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
            } else if (lex_tok == TK_LBRACK) {
                while (lex_tok == TK_LBRACK) {
                    pty = pty + TY_PTR;
                    next();
                    if (lex_tok != TK_RBRACK) parse_const_int();
                    expect(TK_RBRACK);
                }
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
                while (lex_tok == TK_LBRACK) {
                    pty = pty + TY_PTR;
                    next();
                    if (lex_tok != TK_RBRACK) parse_const_int();
                    expect(TK_RBRACK);
                }
                off = add_local(pnm, pty);
                p = nd_var(pnm, off, pty);
                p->is_local = 1;
            }
            if (phead == NULL) {
                phead = p;
                ptail = p;
            } else if (p != NULL) {
                ptail->next = p;
                ptail = p;
            }
            ps_nparams = ps_nparams + 1;
        }
    }
params_done:
    expect(TK_RPAREN);
    skip_gnu_decl_suffixes();

    /* Prototype: type name(params); */
    if (lex_tok == TK_SEMI) {
        next();
        add_func_type(nm, ty);
        return NULL;
    }

    /* Function body */
    if (saw_unnamed_param) {
        p_error("expected param name");
        return NULL;
    }
    add_func_type(nm, ty);
    fn = nd_new(ND_FUNC);
    fn->name = strdup(nm);
    fn->ty = ty;  /* store return type for sema pass */
    fn->args = phead;
    fn->nparams = ps_nparams;
    fn->is_varargs = ps_is_varargs;
    fn->is_static = is_static;
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
    int usize_ty;
    int isize_ty;

    ps_nglobals = 0;
    ps_ginit_pool_len = 0;
    ps_ngirelocs = 0;
#ifdef S12CC_TARGET_A64
    pp_add("__aarch64__", 1);
    pp_add("__LP64__", 1);
#endif
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
    add_typedef("bool", TY_CHAR | TY_UNSIGNED);
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
