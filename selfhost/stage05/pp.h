/* pp.h -- Preprocessor for stage04 compiler
 *
 * Phase 6: #define, #include, #ifdef/#ifndef/#else/#endif, #line skip.
 * Token-level interception: next() calls pp_directive() on TK_HASH,
 * expands macros on TK_IDENT.
 *
 * Compiled by stage03 s32-cc.
 */

#define PP_MAX_DEFS  512
#define PP_MAX_IF    16

static char *pp_dname[PP_MAX_DEFS];
static int   pp_dval[PP_MAX_DEFS];
static char *pp_dbody[PP_MAX_DEFS];   /* body text (strdup'd), NULL = int-only */
static int   pp_dnpar[PP_MAX_DEFS];   /* param count, -1 = object-like */
static char *pp_dparm[PP_MAX_DEFS];   /* packed param names "a\0b\0" (strdup'd) */
static int   pp_dvar[PP_MAX_DEFS];    /* 1 = variadic macro (.../__VA_ARGS__) */
static int   pp_ndefs;

#define PP_EXP_SZ 8192
static char pp_exp[PP_EXP_SZ];

/* Static buffers for pp_define — kept out of stack to avoid
   exceeding the 12-bit addi immediate limit in tree-walk codegen */
static char pp_def_body[4096];
static char pp_def_parms[1024];

static int pp_skip;                /* 1 = skipping tokens (ifdef false branch) */
static int pp_stk[PP_MAX_IF];     /* ifdef nesting: was-skipping before this level */
static int pp_dep;                 /* ifdef nesting depth */
static int pp_taken[PP_MAX_IF];   /* 1 if any branch taken at this depth */

static char pp_sdir[256];         /* source directory prefix for includes */

/* --- Text helpers (read from lex_src[lex_pos]) --- */

static void pp_skip_ws(void) {
    while (lex_src[lex_pos] == 32 || lex_src[lex_pos] == 9) {
        lex_pos = lex_pos + 1;
    }
}

static void pp_skip_line(void) {
    while (lex_pos < lex_len && lex_src[lex_pos] != 10) {
        lex_pos = lex_pos + 1;
    }
}

static void pp_read_name(char *buf) {
    int i;
    int c;
    i = 0;
    c = lex_src[lex_pos];
    while ((c >= 97 && c <= 122) || (c >= 65 && c <= 90) ||
           (c >= 48 && c <= 57) || c == 95) {
        buf[i] = c;
        i = i + 1;
        lex_pos = lex_pos + 1;
        c = lex_src[lex_pos];
    }
    buf[i] = 0;
}

static int pp_read_int(void) {
    int val;
    int neg;
    int c;
    val = 0;
    neg = 0;
    pp_skip_ws();
    if (lex_src[lex_pos] == 45) {  /* '-' */
        neg = 1;
        lex_pos = lex_pos + 1;
        pp_skip_ws();
    }
    c = lex_src[lex_pos];
    /* Hex: 0x... */
    if (c == 48 && (lex_src[lex_pos + 1] == 120 || lex_src[lex_pos + 1] == 88)) {
        lex_pos = lex_pos + 2;
        c = lex_src[lex_pos];
        while ((c >= 48 && c <= 57) || (c >= 97 && c <= 102) || (c >= 65 && c <= 70)) {
            if (c >= 97) {
                val = val * 16 + (c - 97 + 10);
            } else if (c >= 65) {
                val = val * 16 + (c - 65 + 10);
            } else {
                val = val * 16 + (c - 48);
            }
            lex_pos = lex_pos + 1;
            c = lex_src[lex_pos];
        }
    } else {
        /* Decimal */
        while (c >= 48 && c <= 57) {
            val = val * 10 + (c - 48);
            lex_pos = lex_pos + 1;
            c = lex_src[lex_pos];
        }
    }
    /* Skip U/L suffixes */
    c = lex_src[lex_pos];
    while (c == 85 || c == 117 || c == 76 || c == 108) {
        lex_pos = lex_pos + 1;
        c = lex_src[lex_pos];
    }
    if (neg) val = 0 - val;
    return val;
}

/* --- Define table --- */

static int pp_find(char *name) {
    int i;
    i = pp_ndefs - 1;
    while (i >= 0) {
        if (pp_dname[i] != 0 && strcmp(name, pp_dname[i]) == 0) return i;
        i = i - 1;
    }
    return -1;
}

static void pp_add(char *name, int val) {
    int i;
    i = pp_find(name);
    if (i >= 0) {
        pp_dval[i] = val;
        if (pp_dbody[i] != 0) { free(pp_dbody[i]); pp_dbody[i] = 0; }
        if (pp_dparm[i] != 0) { free(pp_dparm[i]); pp_dparm[i] = 0; }
        pp_dnpar[i] = -1;
        pp_dvar[i] = 0;
        return;
    }
    if (pp_ndefs >= PP_MAX_DEFS) {
        fputs("s12cc: too many #defines\n", stderr);
        exit(1);
    }
    pp_dname[pp_ndefs] = strdup(name);
    pp_dval[pp_ndefs] = val;
    pp_dbody[pp_ndefs] = 0;
    pp_dnpar[pp_ndefs] = -1;
    pp_dparm[pp_ndefs] = 0;
    pp_dvar[pp_ndefs] = 0;
    pp_ndefs = pp_ndefs + 1;
}

static void pp_add_func(char *name, int npar, char *parms, int parm_len,
                         char *body) {
    int idx;
    int i;
    pp_add(name, 0);
    idx = pp_find(name);
    pp_dbody[idx] = strdup(body);
    pp_dnpar[idx] = npar;
    if (parm_len > 0) {
        pp_dparm[idx] = malloc(parm_len);
        i = 0;
        while (i < parm_len) {
            pp_dparm[idx][i] = parms[i];
            i = i + 1;
        }
    }
}

/* --- Sync Ragel pointers after lex_pos/lex_len change --- */

static void pp_sync(void) {
    lex_rp = lex_src + lex_pos;
    lex_rpe = lex_src + lex_len;
}

/* --- Macro expansion --- */

static int pp_expand_func(int di) {
    int pos;
    int ins_pos;
    int depth;
    int nargs;
    int arg_start[16];
    int arg_len[16];
    int exp_len;
    char *body;
    char *dp;
    int pi;
    int ni;
    char iname[256];
    int c;
    int j;
    int k;
    int matched;
    int remove_len;
    int delta;
    int tail_len;
    int i;

    /* pos starts at current Ragel scan position (after identifier) */
    pos = lex_rp - lex_src;

    /* Skip whitespace */
    while (lex_src[pos] == 32 || lex_src[pos] == 9) pos = pos + 1;

    /* If no '(', not an invocation */
    if (lex_src[pos] != 40) return 0;

    ins_pos = pos;
    pos = pos + 1;  /* skip '(' */

    /* Read arguments */
    nargs = 0;
    depth = 0;

    /* Skip whitespace before first arg */
    while (lex_src[pos] == 32 || lex_src[pos] == 9) pos = pos + 1;

    if (lex_src[pos] == 41) {  /* ')' — zero args */
        pos = pos + 1;
    } else {
        arg_start[0] = pos;
        while (1) {
            c = lex_src[pos];
            if (c == 0) break;
            if (c == 40) {  /* '(' */
                depth = depth + 1;
                pos = pos + 1;
            } else if (c == 41) {  /* ')' */
                if (depth == 0) {
                    arg_len[nargs] = pos - arg_start[nargs];
                    while (arg_len[nargs] > 0 &&
                           (lex_src[arg_start[nargs] + arg_len[nargs] - 1] == 32 ||
                            lex_src[arg_start[nargs] + arg_len[nargs] - 1] == 9)) {
                        arg_len[nargs] = arg_len[nargs] - 1;
                    }
                    nargs = nargs + 1;
                    pos = pos + 1;
                    break;
                }
                depth = depth - 1;
                pos = pos + 1;
            } else if (c == 44 && depth == 0) {  /* ',' at depth 0 */
                arg_len[nargs] = pos - arg_start[nargs];
                while (arg_len[nargs] > 0 &&
                       (lex_src[arg_start[nargs] + arg_len[nargs] - 1] == 32 ||
                        lex_src[arg_start[nargs] + arg_len[nargs] - 1] == 9)) {
                    arg_len[nargs] = arg_len[nargs] - 1;
                }
                nargs = nargs + 1;
                pos = pos + 1;
                while (lex_src[pos] == 32 || lex_src[pos] == 9) pos = pos + 1;
                if (nargs < 16) arg_start[nargs] = pos;
            } else {
                pos = pos + 1;
            }
        }
    }

    /* Build expanded text in pp_exp */
    body = pp_dbody[di];
    exp_len = 0;
    j = 0;
    while (body[j] != 0) {
        c = body[j];
        if ((c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c == 95) {
            /* Read identifier from body */
            ni = 0;
            while ((c >= 97 && c <= 122) || (c >= 65 && c <= 90) ||
                   (c >= 48 && c <= 57) || c == 95) {
                iname[ni] = c;
                ni = ni + 1;
                j = j + 1;
                c = body[j];
            }
            iname[ni] = 0;

            /* Check if it's a parameter name */
            matched = -1;
            dp = pp_dparm[di];
            if (dp != 0) {
                pi = 0;
                k = 0;
                while (pi < pp_dnpar[di]) {
                    if (strcmp(iname, dp + k) == 0) {
                        matched = pi;
                        break;
                    }
                    while (dp[k] != 0) k = k + 1;
                    k = k + 1;
                    pi = pi + 1;
                }
            }

            if (matched >= 0 && matched < nargs) {
                /* Substitute argument text */
                k = 0;
                while (k < arg_len[matched]) {
                    if (exp_len < PP_EXP_SZ - 1) {
                        pp_exp[exp_len] = lex_src[arg_start[matched] + k];
                        exp_len = exp_len + 1;
                    }
                    k = k + 1;
                }
            } else if (pp_dvar[di] && strcmp(iname, "__VA_ARGS__") == 0) {
                /* Substitute variadic arguments (all args from npar onwards) */
                int va;
                va = pp_dnpar[di];
                while (va < nargs) {
                    if (va > pp_dnpar[di]) {
                        /* Insert ", " separator between variadic args */
                        if (exp_len < PP_EXP_SZ - 1) {
                            pp_exp[exp_len] = 44; exp_len = exp_len + 1;
                        }
                        if (exp_len < PP_EXP_SZ - 1) {
                            pp_exp[exp_len] = 32; exp_len = exp_len + 1;
                        }
                    }
                    k = 0;
                    while (k < arg_len[va]) {
                        if (exp_len < PP_EXP_SZ - 1) {
                            pp_exp[exp_len] = lex_src[arg_start[va] + k];
                            exp_len = exp_len + 1;
                        }
                        k = k + 1;
                    }
                    va = va + 1;
                }
            } else {
                /* Copy identifier as-is */
                k = 0;
                while (iname[k] != 0) {
                    if (exp_len < PP_EXP_SZ - 1) {
                        pp_exp[exp_len] = iname[k];
                        exp_len = exp_len + 1;
                    }
                    k = k + 1;
                }
            }
        } else {
            if (exp_len < PP_EXP_SZ - 1) {
                pp_exp[exp_len] = c;
                exp_len = exp_len + 1;
            }
            j = j + 1;
        }
    }
    pp_exp[exp_len] = 0;

    /* Splice pp_exp into lex_src, replacing lex_src[ins_pos..pos) */
    remove_len = pos - ins_pos;
    delta = exp_len - remove_len;
    tail_len = lex_len - pos;

    if (delta > 0) {
        i = tail_len - 1;
        while (i >= 0) {
            lex_src[pos + delta + i] = lex_src[pos + i];
            i = i - 1;
        }
    } else if (delta < 0) {
        i = 0;
        while (i < tail_len) {
            lex_src[pos + delta + i] = lex_src[pos + i];
            i = i + 1;
        }
    }

    i = 0;
    while (i < exp_len) {
        lex_src[ins_pos + i] = pp_exp[i];
        i = i + 1;
    }

    lex_len = lex_len + delta;
    lex_src[lex_len] = 0;
    lex_pos = ins_pos;
    pp_sync();
    return 1;
}

static void pp_expand_obj(int di) {
    int ins_pos;
    int body_len;
    int tail_len;
    int i;
    char *body;

    body = pp_dbody[di];
    body_len = 0;
    while (body[body_len] != 0) body_len = body_len + 1;

    ins_pos = lex_rp - lex_src;
    tail_len = lex_len - ins_pos;

    /* Shift tail right by body_len */
    i = tail_len - 1;
    while (i >= 0) {
        lex_src[ins_pos + body_len + i] = lex_src[ins_pos + i];
        i = i - 1;
    }

    /* Copy body text */
    i = 0;
    while (i < body_len) {
        lex_src[ins_pos + i] = body[i];
        i = i + 1;
    }

    lex_len = lex_len + body_len;
    lex_src[lex_len] = 0;
    lex_pos = ins_pos;
    pp_sync();
}

/* --- Directives --- */

static void pp_define(void) {
    char name[256];
    int npar;
    int val;
    int c;
    int bi;
    int pi;
    int is_var;

    pp_skip_ws();
    pp_read_name(name);
    if (name[0] == 0) {
        pp_skip_line();
        return;
    }

    /* Function-like macro: name followed by '(' with no space */
    if (lex_src[lex_pos] == 40) {  /* '(' */
        lex_pos = lex_pos + 1;  /* skip '(' */
        npar = 0;
        pi = 0;
        is_var = 0;
        pp_skip_ws();
        if (lex_src[lex_pos] != 41) {  /* not ')' — has params */
            while (1) {
                pp_skip_ws();
                /* Check for variadic '...' */
                if (lex_src[lex_pos] == 46 && lex_src[lex_pos + 1] == 46 &&
                    lex_src[lex_pos + 2] == 46) {
                    lex_pos = lex_pos + 3;
                    is_var = 1;
                    break;
                }
                c = lex_src[lex_pos];
                while ((c >= 97 && c <= 122) || (c >= 65 && c <= 90) ||
                       (c >= 48 && c <= 57) || c == 95) {
                    pp_def_parms[pi] = c;
                    pi = pi + 1;
                    lex_pos = lex_pos + 1;
                    c = lex_src[lex_pos];
                }
                pp_def_parms[pi] = 0;
                pi = pi + 1;
                npar = npar + 1;
                pp_skip_ws();
                if (lex_src[lex_pos] == 44) {  /* ',' */
                    lex_pos = lex_pos + 1;
                } else {
                    break;
                }
            }
        }
        pp_skip_ws();
        if (lex_src[lex_pos] == 41) {  /* ')' */
            lex_pos = lex_pos + 1;
        }
        pp_skip_ws();
        bi = 0;
        while (lex_pos < lex_len && lex_src[lex_pos] != 10 &&
               lex_src[lex_pos] != 13) {
            pp_def_body[bi] = lex_src[lex_pos];
            bi = bi + 1;
            lex_pos = lex_pos + 1;
        }
        while (bi > 0 && (pp_def_body[bi - 1] == 32 || pp_def_body[bi - 1] == 9)) {
            bi = bi - 1;
        }
        pp_def_body[bi] = 0;
        pp_add_func(name, npar, pp_def_parms, pi, pp_def_body);
        if (is_var) {
            pp_dvar[pp_find(name)] = 1;
        }
        pp_skip_line();
        pp_sync();
        return;
    }

    pp_skip_ws();
    c = lex_src[lex_pos];
    /* Check if there's a value: digit, minus, or 0x */
    if ((c >= 48 && c <= 57) || c == 45) {
        val = pp_read_int();
    } else if (c == 10 || c == 13 || c == 0) {
        /* No value — define as 1 */
        val = 1;
    } else {
        /* Non-numeric value — store as text body */
        bi = 0;
        while (lex_pos < lex_len && lex_src[lex_pos] != 10 &&
               lex_src[lex_pos] != 13) {
            pp_def_body[bi] = lex_src[lex_pos];
            bi = bi + 1;
            lex_pos = lex_pos + 1;
        }
        while (bi > 0 && (pp_def_body[bi - 1] == 32 || pp_def_body[bi - 1] == 9)) {
            bi = bi - 1;
        }
        pp_def_body[bi] = 0;
        pp_add(name, 0);
        pp_dbody[pp_find(name)] = strdup(pp_def_body);
        pp_skip_line();
        pp_sync();
        return;
    }
    pp_skip_line();
    pp_add(name, val);
    pp_sync();
}

static void pp_undef(void) {
    char name[256];
    int i;
    pp_skip_ws();
    pp_read_name(name);
    if (name[0] == 0) {
        pp_skip_line();
        pp_sync();
        return;
    }
    i = pp_find(name);
    if (i >= 0) {
        free(pp_dname[i]);
        pp_dname[i] = 0;
        if (pp_dbody[i] != 0) { free(pp_dbody[i]); pp_dbody[i] = 0; }
        if (pp_dparm[i] != 0) { free(pp_dparm[i]); pp_dparm[i] = 0; }
        pp_dnpar[i] = -1;
    }
    pp_skip_line();
    pp_sync();
}

static void pp_include(void) {
    char path[512];
    int pi;
    int fd;
    int n;
    int tail_len;
    int i;
    char *tmp;

    pp_skip_ws();
    if (lex_src[lex_pos] != 34) {  /* '"' */
        /* Not a quoted include — skip (e.g. <...>) */
        pp_skip_line();
        pp_sync();
        return;
    }
    lex_pos = lex_pos + 1;  /* skip opening quote */

    /* Build path: pp_sdir + filename */
    pi = 0;
    i = 0;
    while (pp_sdir[i] != 0) {
        path[pi] = pp_sdir[i];
        pi = pi + 1;
        i = i + 1;
    }
    while (lex_src[lex_pos] != 34 && lex_src[lex_pos] != 0 && lex_src[lex_pos] != 10) {
        path[pi] = lex_src[lex_pos];
        pi = pi + 1;
        lex_pos = lex_pos + 1;
    }
    path[pi] = 0;
    if (lex_src[lex_pos] == 34) {
        lex_pos = lex_pos + 1;  /* skip closing quote */
    }
    pp_skip_line();  /* skip rest of line */

    /* Read file */
    fd = open(path, 0);
    if (fd < 0) {
        fputs("s12cc: cannot open include: ", stderr);
        fputs(path, stderr);
        fputc(10, stderr);
        exit(1);
    }
    tmp = malloc(LEX_SRC_SZ);
    if (!tmp) {
        fputs("s12cc: out of memory for include\n", stderr);
        exit(1);
    }
    n = read(fd, tmp, LEX_SRC_SZ - lex_len - 1);
    close(fd);
    if (n < 0) n = 0;

    /* Splice: shift tail right by n, insert file content */
    tail_len = lex_len - lex_pos;
    /* Backward copy to avoid overlap */
    i = tail_len - 1;
    while (i >= 0) {
        lex_src[lex_pos + n + i] = lex_src[lex_pos + i];
        i = i - 1;
    }
    /* Copy file content */
    i = 0;
    while (i < n) {
        lex_src[lex_pos + i] = tmp[i];
        i = i + 1;
    }
    lex_len = lex_len + n;
    lex_src[lex_len] = 0;
    free(tmp);
    pp_sync();
}

/* --- #if expression evaluator --- */

/* Forward declarations for recursive descent */
static int pp_ev_or(void);

static int pp_ev_primary(void) {
    int val;
    int c;
    char name[256];
    pp_skip_ws();
    c = lex_src[lex_pos];
    /* Parenthesized expression */
    if (c == 40) {  /* '(' */
        lex_pos = lex_pos + 1;
        val = pp_ev_or();
        pp_skip_ws();
        if (lex_src[lex_pos] == 41) {  /* ')' */
            lex_pos = lex_pos + 1;
        }
        return val;
    }
    /* Integer literal (decimal or hex) */
    if ((c >= 48 && c <= 57) || c == 45) {
        return pp_read_int();
    }
    /* Character constant: 'x' */
    if (c == 39) {  /* single quote */
        lex_pos = lex_pos + 1;
        val = lex_src[lex_pos];
        lex_pos = lex_pos + 1;
        if (lex_src[lex_pos] == 39) {
            lex_pos = lex_pos + 1;
        }
        return val;
    }
    /* Identifier: 'defined' or macro name */
    if ((c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c == 95) {
        pp_read_name(name);
        if (strcmp(name, "defined") == 0) {
            int has_paren;
            pp_skip_ws();
            has_paren = 0;
            if (lex_src[lex_pos] == 40) {  /* '(' */
                has_paren = 1;
                lex_pos = lex_pos + 1;
                pp_skip_ws();
            }
            pp_read_name(name);
            if (has_paren) {
                pp_skip_ws();
                if (lex_src[lex_pos] == 41) {  /* ')' */
                    lex_pos = lex_pos + 1;
                }
            }
            if (pp_find(name) >= 0) return 1;
            return 0;
        }
        /* Regular macro — expand to value, or 0 if undefined */
        val = pp_find(name);
        if (val >= 0) return pp_dval[val];
        return 0;
    }
    /* Unknown — return 0 */
    return 0;
}

static int pp_ev_unary(void) {
    int c;
    pp_skip_ws();
    c = lex_src[lex_pos];
    if (c == 33) {  /* '!' */
        lex_pos = lex_pos + 1;
        return !pp_ev_unary();
    }
    if (c == 126) {  /* '~' */
        lex_pos = lex_pos + 1;
        return ~pp_ev_unary();
    }
    if (c == 45) {  /* '-' — only if not a digit (handled by pp_read_int) */
        /* Check if next char is digit — let pp_ev_primary handle negative literals */
        if (lex_src[lex_pos + 1] >= 48 && lex_src[lex_pos + 1] <= 57) {
            return pp_ev_primary();
        }
        lex_pos = lex_pos + 1;
        return 0 - pp_ev_unary();
    }
    if (c == 43) {  /* '+' */
        lex_pos = lex_pos + 1;
        return pp_ev_unary();
    }
    return pp_ev_primary();
}

static int pp_ev_mul(void) {
    int val;
    int c;
    val = pp_ev_unary();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 42 || c == 47 || c == 37) {  /* '*', '/', '%' */
        lex_pos = lex_pos + 1;
        if (c == 42) val = val * pp_ev_unary();
        else if (c == 47) {
            int d;
            d = pp_ev_unary();
            if (d != 0) val = val / d;
            else val = 0;
        } else {
            int d;
            d = pp_ev_unary();
            if (d != 0) val = val % d;
            else val = 0;
        }
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static int pp_ev_add(void) {
    int val;
    int c;
    val = pp_ev_mul();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 43 || c == 45) {  /* '+', '-' */
        lex_pos = lex_pos + 1;
        if (c == 43) val = val + pp_ev_mul();
        else val = val - pp_ev_mul();
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static int pp_ev_shift(void) {
    int val;
    int c;
    val = pp_ev_add();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while ((c == 60 && lex_src[lex_pos + 1] == 60) ||
           (c == 62 && lex_src[lex_pos + 1] == 62)) {  /* '<<', '>>' */
        lex_pos = lex_pos + 2;
        if (c == 60) val = val << pp_ev_add();
        else val = val >> pp_ev_add();
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static int pp_ev_rel(void) {
    int val;
    int c;
    int c2;
    val = pp_ev_shift();
    pp_skip_ws();
    c = lex_src[lex_pos];
    c2 = lex_src[lex_pos + 1];
    while ((c == 60 && c2 != 60 && c2 != 61) ||  /* '<' but not '<<' or '<=' */
           (c == 62 && c2 != 62 && c2 != 61) ||  /* '>' but not '>>' or '>=' */
           (c == 60 && c2 == 61) ||               /* '<=' */
           (c == 62 && c2 == 61)) {               /* '>=' */
        if (c == 60 && c2 == 61) {
            lex_pos = lex_pos + 2;
            val = val <= pp_ev_shift();
        } else if (c == 62 && c2 == 61) {
            lex_pos = lex_pos + 2;
            val = val >= pp_ev_shift();
        } else if (c == 60) {
            lex_pos = lex_pos + 1;
            val = val < pp_ev_shift();
        } else {
            lex_pos = lex_pos + 1;
            val = val > pp_ev_shift();
        }
        pp_skip_ws();
        c = lex_src[lex_pos];
        c2 = lex_src[lex_pos + 1];
    }
    return val;
}

static int pp_ev_eq(void) {
    int val;
    int c;
    val = pp_ev_rel();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while ((c == 61 && lex_src[lex_pos + 1] == 61) ||  /* '==' */
           (c == 33 && lex_src[lex_pos + 1] == 61)) {  /* '!=' */
        lex_pos = lex_pos + 2;
        if (c == 61) val = val == pp_ev_rel();
        else val = val != pp_ev_rel();
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static int pp_ev_bitand(void) {
    int val;
    int c;
    val = pp_ev_eq();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 38 && lex_src[lex_pos + 1] != 38) {  /* '&' but not '&&' */
        lex_pos = lex_pos + 1;
        val = val & pp_ev_eq();
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static int pp_ev_bitxor(void) {
    int val;
    int c;
    val = pp_ev_bitand();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 94) {  /* '^' */
        lex_pos = lex_pos + 1;
        val = val ^ pp_ev_bitand();
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static int pp_ev_bitor(void) {
    int val;
    int c;
    val = pp_ev_bitxor();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 124 && lex_src[lex_pos + 1] != 124) {  /* '|' but not '||' */
        lex_pos = lex_pos + 1;
        val = val | pp_ev_bitxor();
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static int pp_ev_and(void) {
    int val;
    int c;
    val = pp_ev_bitor();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 38 && lex_src[lex_pos + 1] == 38) {  /* '&&' */
        lex_pos = lex_pos + 2;
        val = pp_ev_bitor() && val;
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static int pp_ev_or(void) {
    int val;
    int c;
    val = pp_ev_and();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 124 && lex_src[lex_pos + 1] == 124) {  /* '||' */
        lex_pos = lex_pos + 2;
        val = pp_ev_and() || val;
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static int pp_ev_expr(void) {
    int val;
    val = pp_ev_or();
    pp_skip_line();
    pp_sync();
    return val;
}

/* --- Directives --- */

static void pp_directive(void) {
    char name[256];

    pp_skip_ws();
    pp_read_name(name);

    /* When skipping, only track nesting and check elif/else transitions */
    if (pp_skip) {
        if (strcmp(name, "ifdef") == 0 || strcmp(name, "ifndef") == 0 ||
            strcmp(name, "if") == 0) {
            if (pp_dep >= PP_MAX_IF) {
                fputs("s12cc: #if nesting too deep\n", stderr);
                exit(1);
            }
            pp_stk[pp_dep] = pp_skip;
            pp_taken[pp_dep] = 1;  /* suppress nested elif evaluation */
            pp_dep = pp_dep + 1;
            /* Stay in skip mode */
        } else if (strcmp(name, "elif") == 0) {
            /* Only eval if outer wasn't skipping AND no branch taken yet */
            if (pp_dep > 0 && pp_stk[pp_dep - 1] == 0 &&
                pp_taken[pp_dep - 1] == 0) {
                if (pp_ev_expr()) {
                    pp_taken[pp_dep - 1] = 1;
                    pp_skip = 0;
                }
                return;  /* pp_ev_expr already did skip_line + sync */
            }
        } else if (strcmp(name, "else") == 0) {
            /* Only enter else if outer wasn't skipping AND no branch taken */
            if (pp_dep > 0 && pp_stk[pp_dep - 1] == 0 &&
                pp_taken[pp_dep - 1] == 0) {
                pp_taken[pp_dep - 1] = 1;
                pp_skip = 0;
            }
        } else if (strcmp(name, "endif") == 0) {
            if (pp_dep > 0) {
                pp_dep = pp_dep - 1;
                pp_skip = pp_stk[pp_dep];
            }
        }
        /* undef and other directives: ignore while skipping */
        pp_skip_line();
        pp_sync();
        return;
    }

    /* Normal (not skipping) directive processing */
    if (strcmp(name, "define") == 0) {
        pp_define();
        return;
    }
    if (strcmp(name, "undef") == 0) {
        pp_undef();
        return;
    }
    if (strcmp(name, "include") == 0) {
        pp_include();
        return;
    }
    if (strcmp(name, "if") == 0) {
        int val;
        if (pp_dep >= PP_MAX_IF) {
            fputs("s12cc: #if nesting too deep\n", stderr);
            exit(1);
        }
        pp_stk[pp_dep] = pp_skip;
        pp_taken[pp_dep] = 0;
        val = pp_ev_expr();  /* evals, skip_line, sync */
        if (val) {
            pp_taken[pp_dep] = 1;
        } else {
            pp_skip = 1;
        }
        pp_dep = pp_dep + 1;
        return;
    }
    if (strcmp(name, "ifdef") == 0) {
        pp_skip_ws();
        pp_read_name(name);
        if (pp_dep >= PP_MAX_IF) {
            fputs("s12cc: #ifdef nesting too deep\n", stderr);
            exit(1);
        }
        pp_stk[pp_dep] = pp_skip;
        pp_taken[pp_dep] = 0;
        if (pp_find(name) >= 0) {
            pp_taken[pp_dep] = 1;
        } else {
            pp_skip = 1;
        }
        pp_dep = pp_dep + 1;
        pp_skip_line();
        pp_sync();
        return;
    }
    if (strcmp(name, "ifndef") == 0) {
        pp_skip_ws();
        pp_read_name(name);
        if (pp_dep >= PP_MAX_IF) {
            fputs("s12cc: #ifdef nesting too deep\n", stderr);
            exit(1);
        }
        pp_stk[pp_dep] = pp_skip;
        pp_taken[pp_dep] = 0;
        if (pp_find(name) < 0) {
            pp_taken[pp_dep] = 1;
        } else {
            pp_skip = 1;
        }
        pp_dep = pp_dep + 1;
        pp_skip_line();
        pp_sync();
        return;
    }
    if (strcmp(name, "elif") == 0) {
        /* We're in a taken branch — skip remaining elif/else */
        if (pp_dep > 0) {
            pp_taken[pp_dep - 1] = 1;
            pp_skip = 1;
        }
        pp_skip_line();
        pp_sync();
        return;
    }
    if (strcmp(name, "else") == 0) {
        /* We're in a taken branch — skip else */
        if (pp_dep > 0) {
            pp_taken[pp_dep - 1] = 1;
            pp_skip = 1;
        }
        pp_skip_line();
        pp_sync();
        return;
    }
    if (strcmp(name, "endif") == 0) {
        if (pp_dep > 0) {
            pp_dep = pp_dep - 1;
            pp_skip = pp_stk[pp_dep];
        }
        pp_skip_line();
        pp_sync();
        return;
    }

    /* Unknown directive (#line, #pragma, etc.) — skip */
    pp_skip_line();
    pp_sync();
}
