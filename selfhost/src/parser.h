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
static int parse_const_unary(void);
static void next(void);
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

static int is_gnu_inline_ident(void) {
    if (lex_tok != TK_IDENT) return 0;
    if (strcmp(lex_str, "__inline") == 0) return 1;
    if (strcmp(lex_str, "__inline__") == 0) return 1;
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

static void skip_decl_qualifiers(void) {
    while (1) {
        if (lex_tok == TK_CONST || lex_tok == TK_VOLATILE ||
            lex_tok == TK_RESTRICT || lex_tok == TK_INLINE ||
            is_gnu_qual_ident() || is_gnu_extension_ident() ||
            is_gnu_inline_ident()) {
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
static int   ps_lcols[P_MAX_LOCALS];  /* 2D arrays: last-dim count (0 = 1D) */
static int   ps_lsize[P_MAX_LOCALS];  /* total byte size (arrays: elem_sz*count) */
static int   ps_lstatic[P_MAX_LOCALS]; /* 1 = static local, 0 = normal */
static char *ps_lsname[P_MAX_LOCALS];  /* mangled name (static locals only) */
static int   ps_nlocals;
static int   ps_stack;                /* current stack allocation */
static int   ps_nparams;              /* params in current func */
static int   ps_is_varargs;           /* 1 if current func has ... */
static int   ps_struct_ret;           /* 1 if current func returns struct via hidden ptr */
static int   ps_retptr_off;           /* stack offset of hidden __retptr param */
static int   ps_comp_lit_id;          /* unique hidden locals for compound literals */

static char *ps_gname[P_MAX_GLOBALS]; /* global var names */
static int   ps_gtype[P_MAX_GLOBALS]; /* global var types */
static int   ps_gsize[P_MAX_GLOBALS]; /* size in bytes (0=scalar, >0=array) */
static int   ps_ginit[P_MAX_GLOBALS]; /* initial value for scalars */
static int   ps_ginit_hi[P_MAX_GLOBALS]; /* hi word for 64-bit global initializers */
static int   ps_gstr[P_MAX_GLOBALS];  /* string init: pool index, -1 if none */
static int   ps_glocal[P_MAX_GLOBALS]; /* 1 = static local (suppress .global) */
static int   ps_gextern[P_MAX_GLOBALS]; /* 1 = declaration only, no storage */
static int   ps_gcols[P_MAX_GLOBALS];   /* 2D arrays: last-dim count (0 = 1D) */
static int   ps_nglobals;

/* Static local variable state */
static char *ps_cur_func;             /* current function name, NULL outside */
static char  ps_sl_buf[256];          /* scratch buffer for name mangling */
static int   ps_sl_count;             /* global static-local counter */

/* Array/struct initializer bytes for globals */
#define PS_MAX_INIT_POOL 262144  /* doom's tables.c initializes ~80KB of LUTs */
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
static int   ps_girel_add[PS_MAX_INIT_RELOCS]; /* byte addend for GIRELOC_SYMBOL (&arr[k]) */
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
static int   ps_tdarr[PS_MAX_TYPEDEFS];  /* array typedefs: element count (0 = scalar) */
static int   ps_ntypedefs;
/* Set by parse_type: element count when the type came from an array
 * typedef (typedef byte sha1_digest_t[20]); a declaration of that
 * type must create a real array, not the decayed pointer (SHA1_Final
 * wrote its digest through an uninitialized 4-byte 'pointer'). */
static int   ps_type_arrcount;

/* Function return type table (for 64-bit return value tracking) */
#define PS_MAX_FUNCS 4096
static char *ps_fname[PS_MAX_FUNCS];
static int   ps_ftype[PS_MAX_FUNCS];
static int   ps_fvar[PS_MAX_FUNCS];   /* 1 = variadic (affects f64 arg ABI) */
static int   ps_nfuncs;

/* Forward declarations */
static Node *parse_expr(void);
static Node *parse_stmt(void);
static Node *parse_assign(void);
static Node *parse_postfix(void);
static Node *parse_gnu_asm_stmt(void);
static Node *parse_block(void);

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
            /* Dynamic predefs: values track position / primary source path. */
            if (strcmp(lex_str, "__LINE__") == 0) {
                lex_tok = TK_NUM;
                lex_val = lex_line;
                lex_val_hi = 0;
                lex_val_ll = 0;
                lex_val_u = 0;
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
                /* #define values are 32-bit int; clear the 64-bit state
                 * so a preceding 64-bit literal's flags don't bleed in. */
                lex_val_hi = 0;
                lex_val_ll = 0;
                lex_val_u = 0;
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
    if (is_c11_atomic_ident()) return 1;
    if (is_gnu_extension_ident()) return 1;
    if (is_gnu_qual_ident()) return 1;
    if (is_gnu_attr_ident()) return 1;
    if (is_gnu_typeof_ident()) return 1;
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
    ps_tdarr[ps_ntypedefs] = 0;
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

static void add_func_type(char *name, int ty) {
    int i;
    /* Update existing entry if already registered */
    i = ps_nfuncs - 1;
    while (i >= 0) {
        if (strcmp(name, ps_fname[i]) == 0) {
            ps_ftype[i] = ty;
            ps_fvar[i] = ps_is_varargs;
            return;
        }
        i = i - 1;
    }
    if (ps_nfuncs >= PS_MAX_FUNCS) return;
    ps_fname[ps_nfuncs] = strdup(name);
    ps_ftype[ps_nfuncs] = ty;
    ps_fvar[ps_nfuncs] = ps_is_varargs;
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
    stm_type[stm_count] = nested_ty;
    stm_off[stm_count] = base_off;
    stm_is_arr[stm_count] = 0;
                        stm_arr_cols[stm_count] = 0;
    stm_arr_size[stm_count] = 0;
    stm_owner[stm_count] = owner_si;
    stm_synth[stm_count] = 0;
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
            stm_type[stm_count] = stm_type[i];
            stm_off[stm_count] = base_off + stm_off[i];
            stm_is_arr[stm_count] = stm_is_arr[i];
            stm_arr_size[stm_count] = stm_arr_size[i];
            stm_arr_cols[stm_count] = stm_arr_cols[i];
            stm_owner[stm_count] = owner_si;
            stm_synth[stm_count] = 1;
            stm_bit_off[stm_count] = stm_bit_off[i];
            stm_bit_width[stm_count] = stm_bit_width[i];
            stm_count = stm_count + 1;
        }
        i = i + 1;
    }
}

/* --- Bit-field allocation (C99 6.7.2.1) ---
 *
 * State is per-struct: kept in a 3-int array (bs[0]=uoff, bs[1]=usize,
 * bs[2]=ubits) so nested struct definitions don't clobber each other.
 * uoff = -1 means no active storage unit.  Packing is LSB-first
 * (little-endian), matching gcc/clang on slow-32 / x64 / aarch64; no
 * straddling across units.
 *
 * Subsequent bit-fields of the same base type pack into the same unit
 * until either the next field won't fit or the base type's size changes,
 * at which point a fresh unit is opened at the aligned byte cursor.
 *
 * Bundling the unit state keeps every helper's parameter list at or
 * below 7 — the selfhost compiler's stack-arg handling is unreliable
 * past 8 register args, so we stay well clear.
 */
#define BF_UOFF  0
#define BF_USIZE 1
#define BF_UBITS 2

static void bf_alloc(int dty, int width,
                     int *cur_off, int *max_align, int *bs,
                     int *byte_off_out, int *bit_off_out) {
    int u_size;
    int u_align;
    int need_new;
    int aligned;
    u_size = ty_size(dty);
    u_align = ty_align(dty);
    need_new = (bs[BF_UOFF] < 0) || (bs[BF_USIZE] != u_size) ||
               (bs[BF_UBITS] + width > u_size * 8);
    if (need_new) {
        aligned = ((*cur_off + u_align - 1) / u_align) * u_align;
        bs[BF_UOFF]  = aligned;
        bs[BF_USIZE] = u_size;
        bs[BF_UBITS] = 0;
        *cur_off = aligned + u_size;
    }
    if (u_align > *max_align) *max_align = u_align;
    *byte_off_out = bs[BF_UOFF];
    *bit_off_out  = bs[BF_UBITS];
    bs[BF_UBITS]  = bs[BF_UBITS] + width;
}

/* :0 — flush the current unit so the next bit-field starts fresh, and
 * align the byte cursor to dty's alignment if no unit is active. */
static void bf_flush_for_zero(int dty, int *cur_off, int *max_align, int *bs) {
    int u_align;
    int aligned;
    u_align = ty_align(dty);
    if (bs[BF_UOFF] < 0) {
        aligned = ((*cur_off + u_align - 1) / u_align) * u_align;
        *cur_off = aligned;
    }
    if (u_align > *max_align) *max_align = u_align;
    bs[BF_UOFF]  = -1;
    bs[BF_USIZE] = 0;
    bs[BF_UBITS] = 0;
}

/* Close any open unit before a non-bit-field member; cur_off was already
 * advanced past the unit when it was opened, so just reset state. */
static void bf_close(int *bs) {
    bs[BF_UOFF]  = -1;
    bs[BF_USIZE] = 0;
    bs[BF_UBITS] = 0;
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
static int parse_type(void) {
    int ty;
    int si;
    int mty;
    int dty;
    int off;
    int max_sz;
    int arr_count;
    int arr_ndims;
    int arr_last;
    int val;
    int flex_member;
    int first_decl;
    char nm[256];
    int mtdac;
    mtdac = 0;
    (void)mtdac;
    ps_type_arrcount = 0;
    /* Skip const/volatile/signed/restrict qualifiers */
    while (1) {
        if (lex_tok == TK_CONST || lex_tok == TK_VOLATILE ||
            lex_tok == TK_SIGNED || lex_tok == TK_RESTRICT ||
            is_gnu_qual_ident() || is_gnu_extension_ident() ||
            is_gnu_inline_ident()) {
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
            expect(TK_RPAREN);
            while (lex_tok == TK_STAR) { ty = ty + TY_PTR; next(); }
            return ty;
        }
        return parse_type();
    }
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
            int bs[3];      /* bit-field storage-unit state; see bf_alloc */
            next();
            if (si < 0) {
                si = add_struct(nm);
            } else if (st_nfields[si] == 0 && st_size[si] == 0) {
                st_first[si] = stm_count;
            }
            off = 0;
            max_align = 1;
            bs[BF_UOFF]  = -1;
            bs[BF_USIZE] = 0;
            bs[BF_UBITS] = 0;
            while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
                mty = parse_type();
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

                    if (first_decl) {
                        dty = mty;
                        first_decl = 0;
                    } else {
                        dty = mty;
                        while (ty_is_ptr(dty)) dty = ty_deref(dty);
                        while (lex_tok == TK_STAR) { dty = dty + TY_PTR; next(); }
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
                        if (bf_width == 0) {
                            bf_flush_for_zero(dty, &off, &max_align, bs);
                        } else {
                            bf_alloc(dty, bf_width, &off, &max_align, bs,
                                     &bf_byte_off, &bf_bit_off);
                        }
                        goto struct_comma_check;
                    }
                    is_fn_ptr_member = 0;
                    ptr_row_cols = 0;
                    member_ty = dty;
                    skip_gnu_decl_suffixes();
                    if (lex_tok == TK_SEMI && ty_is_struct(member_ty)) {
                        int malign;
                        require_complete_type(member_ty, "incomplete anonymous struct member");
                        bf_close(bs);
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
                     * non-bit-field bf_close/alignment block below — otherwise
                     * each named bit-field would clear the storage-unit state
                     * and start a fresh unit instead of packing. */
                    if (!is_fn_ptr_member && lex_tok == TK_COLON) {
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
                        bf_alloc(dty, bf_width, &off, &max_align, bs,
                                 &bf_byte_off, &bf_bit_off);
                        stm_owner[stm_count] = si;
                        stm_type[stm_count] = dty;
                        stm_is_arr[stm_count] = 0;
                        stm_arr_cols[stm_count] = 0;
                        stm_arr_size[stm_count] = 0;
                        stm_off[stm_count] = bf_byte_off;
                        stm_synth[stm_count] = 0;
                        stm_bit_off[stm_count] = bf_bit_off;
                        stm_bit_width[stm_count] = bf_width;
                        stm_count = stm_count + 1;
                        st_nfields[si] = st_nfields[si] + 1;
                        skip_gnu_decl_suffixes();
                        goto struct_comma_check;
                    }
                    /* Past the bit-field check: this is a regular member.
                     * End any open bit-field unit (cur_off was already
                     * advanced past it) and align off to the member's
                     * natural alignment. */
                    bf_close(bs);
                    {
                        int malign = ty_align(member_ty);
                        if (malign > 1)
                            off = ((off + malign - 1) / malign) * malign;
                        if (malign > max_align) max_align = malign;
                    }
                    /* Check for array member: type name[N][M]...;  A final
                     * unsized member is a C99 flexible array member: it has
                     * array-address semantics but contributes zero bytes to
                     * sizeof(struct). */
                    arr_count = 0;
                    arr_ndims = 0;
                    arr_last = 0;
                    if (mtdac > 0 && lex_tok != TK_LBRACK) {
                        /* typedef byte sha1_digest_t[20]; member of that
                         * type is a real array member (net_defs). */
                        arr_count = mtdac;
                        arr_ndims = 1;
                        member_ty = ty_deref(member_ty);
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
                        arr_ndims = arr_ndims + 1;
                        expect(TK_RBRACK);
                    }
                    if (arr_ndims > 2) {
                        p_error("arrays of more than 2 dimensions unsupported");
                        return TY_INT;
                    }
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
                        stm_is_arr[stm_count] = 1;
                        stm_arr_cols[stm_count] = (arr_ndims == 2) ? arr_last : 0;
                        stm_arr_size[stm_count] = flex_member ? 0 : ty_size(member_ty) * arr_count;
                        stm_off[stm_count] = off;
                        stm_synth[stm_count] = 0;
                        if (!flex_member) off = off + ty_size(member_ty) * arr_count;
                    } else {
                        require_complete_type(member_ty, "incomplete struct member");
                        stm_owner[stm_count] = si;
                        stm_type[stm_count] = member_ty;
                        stm_is_arr[stm_count] = 0;
                        stm_arr_cols[stm_count] = ptr_row_cols;
                        stm_arr_size[stm_count] = 0;
                        stm_off[stm_count] = off;
                        stm_synth[stm_count] = 0;
                        off = off + ty_size(member_ty);
                    }
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
            /* Any trailing bit-field unit is implicitly closed here.  The
             * byte cursor `off` is already past it (bf_alloc advances
             * cur_off when opening the unit), so sizeof is correct. */
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
                    int bf_width;
                    int ptr_row_cols;

                    if (first_decl) {
                        dty = mty;
                        first_decl = 0;
                    } else {
                        dty = mty;
                        while (ty_is_ptr(dty)) dty = ty_deref(dty);
                        while (lex_tok == TK_STAR) { dty = dty + TY_PTR; next(); }
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
                        stm_is_arr[stm_count] = 0;
                        stm_arr_cols[stm_count] = 0;
                        stm_arr_size[stm_count] = 0;
                        stm_off[stm_count] = 0;
                        stm_synth[stm_count] = 0;
                        stm_bit_off[stm_count] = 0;
                        stm_bit_width[stm_count] = bf_width;
                        stm_count = stm_count + 1;
                        st_nfields[si] = st_nfields[si] + 1;
                        if (ty_size(dty) > max_sz) max_sz = ty_size(dty);
                        malign = ty_align(dty);
                        if (malign > max_align) max_align = malign;
                        skip_gnu_decl_suffixes();
                        goto union_comma_check;
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
                        stm_arr_cols[stm_count] = 0;
                        stm_arr_size[stm_count] = ty_size(member_ty) * arr_count;
                        stm_off[stm_count] = 0;
                        stm_synth[stm_count] = 0;
                        if (ty_size(member_ty) * arr_count > max_sz) max_sz = ty_size(member_ty) * arr_count;
                        malign = ty_align(member_ty);
                        if (malign > max_align) max_align = malign;
                    } else {
                        int malign;
                        require_complete_type(member_ty, "incomplete union member");
                        stm_owner[stm_count] = si;
                        stm_type[stm_count] = member_ty;
                        stm_is_arr[stm_count] = 0;
                        stm_arr_cols[stm_count] = ptr_row_cols;
                        stm_arr_size[stm_count] = 0;
                        stm_off[stm_count] = 0;
                        stm_synth[stm_count] = 0;
                        if (ty_size(member_ty) > max_sz) max_sz = ty_size(member_ty);
                        malign = ty_align(member_ty);
                        if (malign > max_align) max_align = malign;
                    }
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
        int tdi;
        tdi = ps_ntypedefs - 1;
        while (tdi >= 0 && strcmp(lex_str, ps_tdname[tdi]) != 0) tdi = tdi - 1;
        if (tdi < 0) { p_error("expected type"); return TY_INT; }
        ty = ps_tdtype[tdi];
        ps_type_arrcount = ps_tdarr[tdi];
        next();
    }
    else { p_error("expected type"); return TY_INT; }
    while (lex_tok == TK_STAR) { ty = ty + TY_PTR; ps_type_arrcount = 0; next(); }
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
    ps_lcols[idx] = 0;
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
    ps_lcols[idx] = 0;
    ps_lsize[idx] = total;
    /* The table is reused across functions: without this clear, a
     * local array landing on a slot a previous function used for a
     * STATIC local inherits ps_lstatic/ps_lsname and silently aliases
     * that function's mangled global (doom: D_BindVariables' buf
     * became D_Display.menuactivestate.1). */
    ps_lstatic[idx] = 0;
    ps_lsname[idx] = NULL;
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
    ps_gcols[idx] = 0;
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
        if (is_type()) {
            ty = parse_type();
            while (lex_tok == TK_STAR) { ty = ty + TY_PTR; next(); }
            skip_decl_qualifiers();
            expect(TK_RPAREN);
            return ty_size(ty);
        }
        /* sizeof(expression) in a constant context -- global initializers
         * like `sizeof(tab) / sizeof(tab[0])` (rogue's mon_table_len).
         * Mirror the expression parser's sizeof: build the node just to
         * read its type/size, then discard it. */
        {
            Node *sn;
            int sv;
            int sli;
            int sgi;
            sn = parse_expr();
            if (sn->kind == ND_VAR && sn->is_array) {
                if (sn->is_local) {
                    sli = find_local(sn->name);
                    sv = (sli >= 0) ? ps_lsize[sli] : ty_size(sn->ty);
                } else {
                    sgi = find_global(sn->name);
                    sv = (sgi >= 0) ? ps_gsize[sgi] : ty_size(sn->ty);
                }
            } else if (sn->kind == ND_MEMBER && sn->is_array) {
                sv = sn->val_hi;
            } else {
                sv = ty_size(sn->ty);
            }
            expect(TK_RPAREN);
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

/* Constant-expression precedence chain: mul < add < shift < and <
 * xor < or.  Doom's headers lean on shifts ((1<<FRACBITS), (7<<29))
 * and masks in constant initializers. */
static int parse_const_add(void) {
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

static int parse_const_shift(void) {
    int v;

    v = parse_const_add();
    while (lex_tok == TK_LSHIFT || lex_tok == TK_RSHIFT) {
        if (lex_tok == TK_LSHIFT) {
            next();
            v = v << parse_const_add();
        } else {
            next();
            v = v >> parse_const_add();
        }
    }
    return v;
}

static int parse_const_band(void) {
    int v;

    v = parse_const_shift();
    while (lex_tok == TK_AMP) {
        next();
        v = v & parse_const_shift();
    }
    return v;
}

static int parse_const_bxor(void) {
    int v;

    v = parse_const_band();
    while (lex_tok == TK_CARET) {
        next();
        v = v ^ parse_const_band();
    }
    return v;
}

/* Parse a compile-time constant integer (for initializers and array sizes) */
static int parse_const_int(void) {
    int v;

    v = parse_const_bxor();
    while (lex_tok == TK_PIPE) {
        next();
        v = v | parse_const_bxor();
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

static int parse_global_init_symbol_reloc_at(int gidx, int rel_off, int sz) {
    char nm[256];
    int ci;
    int amp;
    int gi2;
    int esz;
    int sidx;

    while (try_consume_type_cast()) {
    }
    amp = 0;
    if (lex_tok == TK_AMP) {
        amp = 1;
        next();
        while (try_consume_type_cast()) {
        }
    }
    if (lex_tok != TK_IDENT) {
        if (amp) p_error("expected symbol after & in initializer");
        return 0;
    }
    ci = find_const(lex_str);
    if (ci >= 0) return 0;
    memcpy(nm, lex_str, lex_slen + 1);
    next();
    ps_ginit_add_sym_reloc_at(gidx, rel_off, nm, sz);
    /* &sym[const]: address constant with a byte addend
     * (tables.c: const fixed_t *finecosine = &finesine[FINEANGLES/4]) */
    if (amp && lex_tok == TK_LBRACK) {
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

static void parse_global_init_value(int ty, int gidx);
static void parse_global_init_value_at(int ty, int arr_count, int gidx, int rel_off);
static int parse_global_init_array_at(int elem_ty, int count, int gidx, int base_rel);
static int parse_global_init_array2d_at(int elem_ty, int rows, int cols, int gidx, int base_rel);
static void parse_global_init_struct_at(int ty, int gidx, int base_rel);

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
    while (lex_tok == TK_LBRACK || lex_tok == TK_DOT) {
        if (lex_tok == TK_LBRACK) {
            next();
            idx = parse_const_int();
            if (idx < 0) p_error("negative array designator");
            expect(TK_RBRACK);
            if (!ty_is_ptr(ty) || arr_count == 0)
                p_error("array designator on non-array");
            if (arr_count > 0 && idx >= arr_count) p_error("array designator out of range");
            elem_ty = ty_deref(ty);
            rel = rel + (idx * ty_size(elem_ty));
            ty = elem_ty;
            arr_count = 0;
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
            if (stm_bit_width[mi] > 0)
                p_error("bit-field in struct initializer unsupported");
            rel = rel + stm_off[mi];
            ty = stm_type[mi];
            arr_count = struct_member_array_count(mi);
            if (*root_out < 0) *root_out = struct_member_nth_for_idx(si, mi);
        }
    }
    *rel_out = rel;
    *ty_out = ty;
    *arr_count_out = arr_count;
}

static int parse_global_init_array_at(int elem_ty, int count, int gidx, int base_rel) {
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
    if ((elem_ty & TY_BASE_MASK) == TY_CHAR && lex_tok == TK_STRING) {
        sp_idx = parse_string_literal();
        slen = lex_str_len[sp_idx];
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
                expect(TK_COMMA);
            }
        }
        if (lex_tok == TK_LBRACK) {
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
static int parse_global_init_array2d_at(int elem_ty, int rows, int cols, int gidx, int base_rel) {
    int i;
    int row_sz;

    row_sz = ty_size(elem_ty) * cols;
    expect(TK_LBRACE);
    i = 0;
    while (lex_tok != TK_EOF) {
        if (lex_tok == TK_RBRACE) break;
        if (i > 0) {
            expect(TK_COMMA);
            if (lex_tok == TK_RBRACE) break;
        }
        if (rows >= 0 && i >= rows) p_error("too many 2D initializer rows");
        parse_global_init_array_at(elem_ty, cols, gidx, base_rel + i * row_sz);
        i = i + 1;
    }
    if (lex_tok == TK_COMMA) next();
    expect(TK_RBRACE);
    if (rows < 0) rows = i;
    ps_ginit_ensure_len(gidx, base_rel + rows * row_sz);
    return rows;
}

static void parse_global_init_struct_at(int ty, int gidx, int base_rel) {
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
            parse_global_init_designator(ty, 0, &rel, &target_ty,
                                         &target_arr_count, &root);
            expect(TK_ASSIGN);
            parse_global_init_value_at(target_ty, target_arr_count, gidx, base_rel + rel);
            if (root >= 0) i = root + 1;
            else i = i + 1;
        } else {
            if (i >= nf) p_error("too many initializers");
            mi = struct_field_nth_idx(si, i);
            if (mi < 0) p_error("missing struct field");
            if (struct_member_is_flexible_array(mi))
                p_error("flexible array initializer unsupported");
            if (stm_bit_width[mi] > 0)
                p_error("bit-field in struct initializer unsupported");
            field_ty = stm_type[mi];
            arr_count = struct_member_array_count(mi);
            if (stm_arr_cols[mi] > 0 && lex_tok == TK_LBRACE) {
                /* 2D member array: rows of cols elements */
                parse_global_init_array2d_at(ty_deref(field_ty),
                    arr_count / stm_arr_cols[mi], stm_arr_cols[mi],
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
static void ps_fp_init_store_at(int ty, int gidx, int rel_off) {
    int fneg;
    int flo;
    int fhi;
    int fw[2];
    int i;
    int fop;
    double fd;
    double rv;

    fneg = 0;
    if (lex_tok == TK_MINUS) { fneg = 1; next(); }
    else if (lex_tok == TK_PLUS) next();
    if (lex_tok == TK_FNUM) {
        flo = lex_val;
        fhi = lex_fval_hi;
        next();
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

static void parse_global_init_value_at(int ty, int arr_count, int gidx, int rel_off) {
    int v;
    int sp_idx;

    if (arr_count != 0 && ty_is_ptr(ty)) {
        parse_global_init_array_at(ty_deref(ty), arr_count, gidx, rel_off);
        return;
    }
    if (ty_is_struct(ty)) {
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
    v = parse_const_int();
    ps_ginit_store_int_at(gidx, rel_off, v, ty_size(ty));
}

static void parse_global_init_value(int ty, int gidx) {
    parse_global_init_value_at(ty, 0, gidx, ps_ginit_cur_off(gidx));
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

static Node *parse_compound_literal_expr(int ty, int arr_count);

static Node *parse_primary(void) {
    Node *n;
    int v;
    int ty;
    int arr_count;
    int count2;
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
        int v_hi;
        int v_ll;
        int v_u;
        v    = lex_val;
        v_hi = lex_val_hi;
        v_ll = lex_val_ll;
        v_u  = lex_val_u;
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
            if (v_u) t = t | TY_UNSIGNED;
            return nd_num64(v, v_hi, t);
        }
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
                n->arr_cols = ps_lcols[li];
                return n;
            }
            n = nd_var(nm, ps_loff[li], ps_ltype[li]);
            n->is_local = 1;
            n->is_array = ps_larr[li];
            n->arr_cols = ps_lcols[li];
            return n;
        }

        /* Global variable */
        gi = find_global(nm);
        if (gi >= 0) {
            n = nd_var(nm, 0, ps_gtype[gi]);
            n->is_local = 0;
            n->is_array = (ps_gsize[gi] > 0) ? 1 : 0;
            n->arr_cols = ps_gcols[gi];
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

        /* Bare function name: load address (for function pointers).
         * Require a prior declaration — otherwise a typo'd local/global
         * silently emits an unresolved relocation. */
        if (is_known_func(nm)) {
            return nd_func_ref(nm);
        }
        fdputs("s12cc:", 2);
        fdputuint(2, lex_line);
        fdputs(": error: undeclared identifier '", 2);
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
            arr_count = 0;
            while (lex_tok == TK_LBRACK) {
                next();
                count2 = -1;
                if (lex_tok != TK_RBRACK) count2 = parse_const_int();
                expect(TK_RBRACK);
                if (arr_count == 0) arr_count = count2;
                else {
                    if (arr_count < 0 || count2 < 0)
                        p_error("array size required in compound literal type");
                    arr_count = arr_count * count2;
                }
            }
            expect(TK_RPAREN);
            if (lex_tok == TK_LBRACE) {
                return parse_compound_literal_expr(ty, arr_count);
            }
            if (arr_count != 0) p_error("array cast unsupported");
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
        if (stm_bit_width[omi] > 0) {
            p_error("offsetof of bit-field is not allowed");
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
            } else if (n->kind == ND_BINOP && n->arr_cols > 0 &&
                       ty_is_ptr(n->ty)) {
                /* Row of a pointer-to-array member: N * elem size */
                v = n->arr_cols * ty_size(ty_deref(n->ty));
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
            /* C: (*f)(args) == f(args) for function pointers.  In this
             * subset a function pointer VALUE is int-typed, so the
             * no-op derefs to strip are exactly the STARs whose
             * operand is NOT a real pointer.  A STAR over a pointer
             * operand is a genuine element load (wipes[i] loads the
             * slot; stripping it called the slot's ADDRESS). */
            while (n->kind == ND_UNARY && n->op == TK_STAR &&
                   n->lhs != NULL && !ty_is_ptr(n->lhs->ty))
                n = n->lhs;
            n = nd_call_ptr(n, ahead, anargs);
        } else if (lex_tok == TK_LBRACK) {
            next();
            idx = parse_expr();
            expect(TK_RBRACK);
            if (n->is_array && n->arr_cols > 0) {
                /* First index of a 2D array: row address, no load.
                 * arr[i][j] => *((arr + i*cols) + j); the element-size
                 * scaling of both additions stays in codegen. */
                idx = nd_binop(TK_STAR, idx, nd_num(n->arr_cols));
                n = nd_binop(TK_PLUS, n, idx);
            } else if (n->kind == ND_MEMBER && !n->is_array &&
                       n->arr_cols > 0 && ty_is_ptr(n->ty)) {
                /* Pointer-to-array member: p[i] selects row i — scale
                 * by the row width, keep the value a row pointer (no
                 * element load).  arr_cols is kept on the row node so
                 * sizeof(p[i]) reports the row size. */
                {
                    int row_cols;
                    row_cols = n->arr_cols;
                    idx = nd_binop(TK_STAR, idx, nd_num(row_cols));
                    n = nd_binop(TK_PLUS, n, idx);
                    n->arr_cols = row_cols;
                }
            } else {
                /* n[idx] → *(n + idx)  — codegen handles pointer arithmetic scaling */
                n = nd_binop(TK_PLUS, n, idx);
                n = nd_unary(TK_STAR, n);
            }
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
            n = nd_member(n, stm_off[mi], stm_type[mi], stm_is_arr[mi], stm_arr_size[mi],
                          stm_bit_off[mi], stm_bit_width[mi]);
            n->arr_cols = stm_arr_cols[mi];
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
            n->arr_cols = stm_arr_cols[mi];
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
        if (n->kind == ND_MEMBER && n->bit_width > 0) {
            p_error("cannot take address of bit-field");
        }
        /* &arr2d[i]: the first subscript of a 2D array already yields
         * the row ADDRESS (an ADD node, not an lvalue).  C's pointer-
         * to-row has the same numeric value, so the node stands as-is
         * (m_menu's fread(&savegamestrings[i], ...)).  Plain &(a+b)
         * is not legal C, so this only fires for that shape. */
        if (n->kind == ND_BINOP && n->op == TK_PLUS) {
            return n;
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

static char *ps_li_name;
static int ps_li_off;
static int ps_li_base_ty;
static int ps_li_base_is_array;
static Node *ps_li_head;
static Node *ps_li_tail;

static void local_init_begin(char *nm, int off, int base_ty, int base_is_array) {
    ps_li_name = nm;
    ps_li_off = off;
    ps_li_base_ty = base_ty;
    ps_li_base_is_array = base_is_array;
    ps_li_head = NULL;
    ps_li_tail = NULL;
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

    elem_sz = ty_size(elem_ty);
    if ((elem_ty & TY_BASE_MASK) == TY_CHAR && lex_tok == TK_STRING) {
        sp_idx = parse_string_literal();
        slen = lex_str_len[sp_idx];
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
                expect(TK_COMMA);
            }
        }
        if (lex_tok == TK_LBRACK) {
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

    si = ty_struct_idx(ty);
    nf = st_nfields[si];
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
            parse_global_init_designator(ty, 0, &rel, &target_ty,
                                         &target_arr_count, &root);
            expect(TK_ASSIGN);
            parse_local_init_value_at(target_ty, target_arr_count, rel_off + rel);
            if (root >= 0) i = root + 1;
            else i = i + 1;
        } else {
            if (i >= nf) p_error("too many initializers");
            mi = struct_field_nth_idx(si, i);
            if (mi < 0) p_error("missing struct field");
            if (struct_member_is_flexible_array(mi))
                p_error("flexible array initializer unsupported");
            if (stm_bit_width[mi] > 0)
                p_error("bit-field in struct initializer unsupported");
            field_ty = stm_type[mi];
            field_arr_count = struct_member_array_count(mi);
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
    if (arr_count != 0 && ty_is_ptr(ty)) {
        parse_local_init_array_at(ty_deref(ty), arr_count, rel_off);
        return;
    }
    if (ty_is_struct(ty)) {
        parse_local_init_struct_at(ty, rel_off);
        return;
    }
    local_init_emit_assign(rel_off, ty, parse_assign());
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

static Node *parse_compound_literal_expr(int ty, int arr_count) {
    char nm[256];
    int off;
    Node *head;
    Node *zhead;
    Node *ztail;
    Node *result;

    ps_make_compound_literal_name(nm);
    if (arr_count != 0) {
        local_init_begin(nm, 0, ty + TY_PTR, 1);
        arr_count = parse_local_init_array_at(ty, arr_count, 0);
        head = ps_li_head;
        off = add_local_array(nm, ty, arr_count);
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
        result->is_local = 1;
        result->is_array = 1;
        return local_init_list_expr(head, result);
    }

    off = add_local(nm, ty);
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
    result->is_local = 1;
    return local_init_list_expr(ps_li_head, result);
}

/* --- Statement parser --- */

static Node *parse_block(void);

/* Shared typedef declaration body (after the `typedef` keyword).
 * Called from both file scope and statement scope — vecscope declares
 * `typedef struct {...} key_t;` inside main.  Names register in the
 * file-scope typedef table either way (fine for this C subset). */
static void parse_typedef_decl(void) {
    int ty;
    char nm[256];
        ty = parse_type();
        if (lex_tok == TK_LPAREN) {
            /* Function pointer typedef: typedef int (*Name)(args); */
            next();  /* skip ( */
            if (lex_tok == TK_STAR) next();  /* skip * */
            if (lex_tok != TK_IDENT) {
                p_error("expected typedef name");
                return;
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
            skip_gnu_decl_suffixes();
            expect(TK_SEMI);
            return;
        }
        if (lex_tok == TK_LBRACK) {
            int tdcount;
            next();
            tdcount = 0;
            if (lex_tok != TK_RBRACK) tdcount = parse_const_int();
            expect(TK_RBRACK);
            add_typedef(nm, ty + TY_PTR);
            /* Object declarations of this type must materialize the
             * array; parameters still decay. */
            ps_tdarr[ps_ntypedefs - 1] = tdcount;
            skip_gnu_decl_suffixes();
            expect(TK_SEMI);
            return;
        }
        add_typedef(nm, ty);
        skip_gnu_decl_suffixes();
        expect(TK_SEMI);
        return;
    }

static Node *parse_stmt(void) {
    int tdac;
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

    /* case label: any constant expression (doom packs chars with
     * shifts: case AM_MSGENTERED == (('a'<<24)+...)+('e'<<8)) */
    if (lex_tok == TK_CASE) {
        next();
        neg = 0;
        cv = parse_const_int();
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
    /* Function-scope typedef (vecscope: typedef struct {...} key_t;) */
    if (lex_tok == TK_TYPEDEF) {
        next();
        parse_typedef_decl();
        return nd_block(NULL);
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
        else next();
    }

    /* local variable declaration */
    if (is_type()) {
        ty = parse_type();
        tdac = ps_type_arrcount;
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
            /* Array of function pointers: type (*name[N])(args)
             * (f_wipe's static wipes[] table).  Elements are word-
             * sized; a static one lives in .data with symbol-reloc
             * initializers. */
            if (lex_tok == TK_LBRACK) {
                int fpcount;
                int fpi;
                char fpn[256];
                next();
                fpcount = -1;
                if (lex_tok != TK_RBRACK) fpcount = parse_const_int();
                expect(TK_RBRACK);
                expect(TK_RPAREN);
                if (lex_tok == TK_LPAREN) {
                    next();
                    while (lex_tok != TK_RPAREN && lex_tok != TK_EOF) next();
                    expect(TK_RPAREN);
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
                sl_li = ps_nlocals;
                if (sl_li >= P_MAX_LOCALS) { p_error("too many locals"); return nd_num(0); }
                ps_lname[sl_li] = strdup(nm);
                ps_loff[sl_li] = 0;
                ps_ltype[sl_li] = TY_INT + TY_PTR;
                ps_larr[sl_li] = 1;
                ps_lcols[sl_li] = 0;
                ps_lstatic[sl_li] = 1;
                ps_lsname[sl_li] = strdup(ps_sl_buf);
                ps_nlocals = ps_nlocals + 1;
                return nd_block(NULL);
            }
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

        /* Typedef'd array object: typedef byte sha1_digest_t[20];
         * sha1_digest_t digest;  — materialize the array. */
        if (tdac > 0 && lex_tok == TK_SEMI && !is_extern && !is_static) {
            off = add_local_array(nm, ty_deref(ty), tdac);
            next();
            return nd_block(NULL);
        }

        /* Statement-scope function PROTOTYPE (doom's wi_stuff declares
         * `void WI_unloadData(void);` inside a function).  Skip the
         * parameter list; calls resolve like any direct call. */
        if (lex_tok == TK_LPAREN) {
            int pdepth;
            pdepth = 0;
            next();
            pdepth = 1;
            while (pdepth > 0 && lex_tok != TK_EOF) {
                if (lex_tok == TK_LPAREN) pdepth = pdepth + 1;
                if (lex_tok == TK_RPAREN) pdepth = pdepth - 1;
                next();
            }
            expect(TK_SEMI);
            return nd_block(NULL);
        }

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
                } else {
                    ps_ginit[sl_gi] = parse_const_int();
                }
            }
            /* Register, then loop on comma for further declarators:
             * static int lastlevel = -1, lastepisode = -1; (am_map) */
            while (1) {
                sl_li = ps_nlocals;
                if (sl_li >= P_MAX_LOCALS) { p_error("too many locals"); return nd_num(0); }
                ps_lname[sl_li] = strdup(nm);
                ps_loff[sl_li] = 0;
                ps_ltype[sl_li] = ty;
                ps_larr[sl_li] = 0;
                ps_lstatic[sl_li] = 1;
                ps_lsname[sl_li] = strdup(ps_sl_buf);
                ps_nlocals = ps_nlocals + 1;
                if (lex_tok != TK_COMMA) break;
                next();
                if (lex_tok != TK_IDENT) { p_error("expected name after comma"); return nd_num(0); }
                memcpy(nm, lex_str, lex_slen + 1);
                next();
                ps_mangle_static(ps_cur_func, nm);
                sl_gi = add_global(ps_sl_buf, ty,
                                   ty_is_struct(ty) ? ty_size(ty) : 0);
                ps_glocal[sl_gi] = 1;
                if (lex_tok == TK_ASSIGN) {
                    next();
                    ps_ginit[sl_gi] = parse_const_int();
                }
            }
            expect(TK_SEMI);
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
            lcols2 = 0;
            while (lex_tok == TK_LBRACK) {
                next();
                count2 = -1;
                if (lex_tok != TK_RBRACK) {
                    count2 = parse_const_int();
                }
                expect(TK_RBRACK);
                if (count2 < 0) {
                    p_error("array size required without initializer");
                    return nd_num(0);
                }
                if (lcols2 != 0) {
                    p_error("arrays of more than 2 dimensions unsupported");
                    return nd_num(0);
                }
                lcols2 = count2;
                if (count >= 0) count = count * count2;
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
                ps_gcols[sl_gi] = lcols2;
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
                            (count >= 0) ? count / lcols2 : -1, lcols2, sl_gi, 0);
                        count = count * lcols2;
                    } else {
                        count = parse_global_init_array_at(ty, count, sl_gi, 0);
                    }
                    ps_gsize[sl_gi] = ty_size(ty) * count;
                    ps_ginit_ensure_len(sl_gi, ps_gsize[sl_gi]);
                    ps_ginit_finish(sl_gi);
                }
                expect(TK_SEMI);
                sl_li = ps_nlocals;
                if (sl_li >= P_MAX_LOCALS) { p_error("too many locals"); return nd_num(0); }
                ps_lname[sl_li] = strdup(nm);
                ps_loff[sl_li] = 0;
                ps_ltype[sl_li] = ty + TY_PTR;
                ps_larr[sl_li] = 1;
                ps_lcols[sl_li] = lcols2;
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
                } else if (lex_tok == TK_LBRACE && lcols2 > 0) {
                    /* Local 2D char array with string rows:
                     * char name[23][8] = { "e2m1", ... } (d_main).  Each
                     * row gets its string's bytes, zero-padded to cols;
                     * a string of exactly cols chars drops its NUL (C
                     * semantics — doom's "spida1d1" needs this). */
                    if ((ty & TY_BASE_MASK) != TY_CHAR || (ty & TY_PTR_MASK) != 0 || count < 0) {
                        p_error("2D local array initializers support only char[N][M] with string rows");
                        return nd_num(0);
                    }
                    off = add_local_array(nm, ty, count);
                    ps_lcols[ps_nlocals - 1] = lcols2;
                    head = NULL;
                    tail = NULL;
                    next();
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
                            n->is_local = 1;
                            n->is_array = 1;
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
                    expect(TK_SEMI);
                    if (head != NULL) return nd_block(head);
                    return nd_block(NULL);
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
                /* Comma-separated array declarators:
                 * char lbuf[64], rbuf[64]; (sbasic's eval.c) */
                while (lex_tok == TK_COMMA) {
                    next();
                    if (lex_tok != TK_IDENT) { p_error("expected name after comma"); return nd_num(0); }
                    memcpy(nm, lex_str, lex_slen + 1);
                    next();
                    if (lex_tok != TK_LBRACK) { p_error("expected [ in comma array decl"); return nd_num(0); }
                    next();
                    count = parse_const_int();
                    expect(TK_RBRACK);
                    off = add_local_array(nm, ty, count);
                    ps_lcols[ps_nlocals - 1] = 0;
                }
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
                local_init_begin(nm, off, ty, 0);
                local_init_zero_at(ty, 0, 0);
                parse_local_init_struct_at(ty, 0);
                head = ps_li_head;
                tail = ps_li_tail;
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
    int g2cols;
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
        parse_typedef_decl();
        return NULL;
    }

    /* Enum definition at top level.  `enum {...} name;` also declares
     * an int variable of the anonymous enum type (doom's main_e /
     * specials_e). */
    if (lex_tok == TK_ENUM) {
        next();
        parse_enum_def();
        if (lex_tok == TK_IDENT) {
            memcpy(nm, lex_str, lex_slen + 1);
            next();
            add_defined_global(nm, TY_INT, 0);
        }
        expect(TK_SEMI);
        return NULL;
    }

    /* Parse return type / variable type */
    ty = parse_type();
    g2cols = ps_type_arrcount;  /* reuse: typedef'd array element count */
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
        if (is_static) ps_glocal[idx] = 1; /* file-scope static: TU-local */
        if (lex_tok == TK_ASSIGN) {
            /* fn-ptr global initializer: NULL/0 or a function name
             * (sbasic: env_deftype_hook = NULL) */
            next();
            if (lex_tok == TK_IDENT && find_const(lex_str) < 0 &&
                (is_known_func(lex_str) || find_global(lex_str) >= 0)) {
                ps_ginit_begin(idx);
                parse_global_init_symbol_reloc_at(idx, 0, 4);
                ps_ginit_ensure_len(idx, 4);
                ps_ginit_finish(idx);
            } else {
                ps_ginit[idx] = parse_const_int();
            }
        }
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

    /* Typedef'd array global: sha1_digest_t g;  — real array storage. */
    if (g2cols > 0 && lex_tok == TK_SEMI) {
        if (is_extern) idx = add_extern_global(nm, ty, ty_size(ty_deref(ty)) * g2cols);
        else           idx = add_defined_global(nm, ty, ty_size(ty_deref(ty)) * g2cols);
        if (is_static) ps_glocal[idx] = 1;
        ps_gcols[idx] = 0;
        next();
        return NULL;
    }
    g2cols = 0;

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
                if (lex_tok == TK_LBRACE && ty_is_struct(xty)) {
                    /* Struct initializer: struct S s = { ... }; */
                    ps_ginit_begin(idx);
                    parse_global_init_value(xty, idx);
                    ps_ginit_finish(idx);
                } else if (lex_tok == TK_STRING) {
                    ps_gstr[idx] = parse_string_literal();
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
                             find_global(lex_str) >= 0))) {
                    /* Pointer global initialized with an address constant:
                     * `const fixed_t *finecosine = &finesine[FINEANGLES/4];`
                     * Route through the init-pool reloc machinery. */
                    ps_ginit_begin(idx);
                    if (!parse_global_init_symbol_reloc_at(idx, 0, ty_size(xty)))
                        p_error("bad pointer initializer");
                    ps_ginit_ensure_len(idx, ty_size(xty));
                    ps_ginit_finish(idx);
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
        g2cols = 0;
        while (lex_tok == TK_LBRACK) {
            /* Global 2D array: flatten to rows*cols elements; record the
             * column count so the first subscript scales by a whole row.
             * The first dimension may be omitted when an initializer
             * infers it (T a[][C] = {...}). */
            next();
            if (g2cols != 0) {
                p_error("arrays of more than 2 dimensions unsupported");
                return NULL;
            }
            g2cols = parse_const_int();
            if (count >= 0) count = count * g2cols;
            expect(TK_RBRACK);
        }
        skip_gnu_decl_suffixes();
        if (g2cols > 0 && lex_tok == TK_ASSIGN) {
            next();
            if (lex_tok != TK_LBRACE) {
                p_error("expected { in 2D array init");
                return NULL;
            }
            require_complete_type(ty, "incomplete element type");
            idx = add_defined_global(nm, ty + TY_PTR, 0);
            if (is_static) ps_glocal[idx] = 1; /* file-scope static: TU-local */
            ps_gcols[idx] = g2cols;
            ps_ginit_begin(idx);
            count = parse_global_init_array2d_at(ty, (count >= 0) ? count / g2cols : -1,
                                                 g2cols, idx, 0);
            count = count * g2cols;
            ps_gsize[idx] = ty_size(ty) * count;
            ps_ginit_ensure_len(idx, ps_gsize[idx]);
            ps_ginit_finish(idx);
            expect(TK_SEMI);
            return NULL;
        }
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
                idx = add_defined_global(nm, ty + TY_PTR, 0);
                if (is_static) ps_glocal[idx] = 1; /* file-scope static: TU-local */
                ps_ginit_begin(idx);
                count = parse_global_init_array_at(ty, count, idx, 0);
                require_complete_type(ty, "incomplete element type");
                ps_gsize[idx] = ty_size(ty) * count;
                ps_ginit_ensure_len(idx, ps_gsize[idx]);
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
            if (is_static) ps_glocal[idx] = 1; /* file-scope static: TU-local */
            ps_gcols[idx] = g2cols;
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
        skip_gnu_decl_suffixes();
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
            skip_gnu_decl_suffixes();
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
    ps_comp_lit_id = 0;
    pp_install_predefs();
#ifdef S12CC_TARGET_A64
    pp_add("__aarch64__", 1);
    pp_add("__LP64__", 1);
#endif
#ifdef S12CC_TARGET_X64
    pp_add("__x86_64__", 1);
    pp_add("__LP64__", 1);
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
