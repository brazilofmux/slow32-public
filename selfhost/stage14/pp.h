/* pp.h -- Preprocessor for stage12 compiler
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
static int   pp_ndefs;

static int pp_skip;                /* 1 = skipping tokens (ifdef false branch) */
static int pp_stk[PP_MAX_IF];     /* ifdef nesting: was-skipping before this level */
static int pp_dep;                 /* ifdef nesting depth */

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
        if (strcmp(name, pp_dname[i]) == 0) return i;
        i = i - 1;
    }
    return -1;
}

static void pp_add(char *name, int val) {
    int i;
    i = pp_find(name);
    if (i >= 0) {
        pp_dval[i] = val;
        return;
    }
    if (pp_ndefs >= PP_MAX_DEFS) {
        fputs("s12cc: too many #defines\n", stderr);
        exit(1);
    }
    pp_dname[pp_ndefs] = strdup(name);
    pp_dval[pp_ndefs] = val;
    pp_ndefs = pp_ndefs + 1;
}

/* --- Sync Ragel pointers after lex_pos/lex_len change --- */

static void pp_sync(void) {
    lex_rp = lex_src + lex_pos;
    lex_rpe = lex_src + lex_len;
}

/* --- Directives --- */

static void pp_define(void) {
    char name[256];
    int val;
    int c;
    pp_skip_ws();
    pp_read_name(name);
    if (name[0] == 0) {
        pp_skip_line();
        return;
    }
    /* Skip function-like macros: name followed by '(' with no space */
    if (lex_src[lex_pos] == 40) {
        pp_skip_line();
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
        /* Non-numeric value (e.g. expression, string) — skip, define as 0 */
        pp_skip_line();
        val = 0;
        pp_add(name, val);
        pp_sync();
        return;
    }
    pp_skip_line();
    pp_add(name, val);
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

static void pp_directive(void) {
    char name[256];

    pp_skip_ws();
    pp_read_name(name);

    /* When skipping, only track ifdef nesting */
    if (pp_skip) {
        if (strcmp(name, "ifdef") == 0 || strcmp(name, "ifndef") == 0) {
            if (pp_dep >= PP_MAX_IF) {
                fputs("s12cc: #ifdef nesting too deep\n", stderr);
                exit(1);
            }
            pp_stk[pp_dep] = pp_skip;
            pp_dep = pp_dep + 1;
            /* Stay in skip mode */
        } else if (strcmp(name, "else") == 0) {
            /* Only toggle at current depth if outer wasn't skipping */
            if (pp_dep > 0 && pp_stk[pp_dep - 1] == 0) {
                pp_skip = 0;
            }
        } else if (strcmp(name, "endif") == 0) {
            if (pp_dep > 0) {
                pp_dep = pp_dep - 1;
                pp_skip = pp_stk[pp_dep];
            }
        }
        pp_skip_line();
        pp_sync();
        return;
    }

    /* Normal (not skipping) directive processing */
    if (strcmp(name, "define") == 0) {
        pp_define();
        return;
    }
    if (strcmp(name, "include") == 0) {
        pp_include();
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
        pp_dep = pp_dep + 1;
        if (pp_find(name) < 0) {
            pp_skip = 1;
        }
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
        pp_dep = pp_dep + 1;
        if (pp_find(name) >= 0) {
            pp_skip = 1;
        }
        pp_skip_line();
        pp_sync();
        return;
    }
    if (strcmp(name, "else") == 0) {
        if (pp_dep > 0) {
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
