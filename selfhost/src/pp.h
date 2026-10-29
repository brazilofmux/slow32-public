/* pp.h -- Preprocessor for the live selfhost frontend (stage08 + cross)
 *
 * Directives: #define (object + function-like + __VA_ARGS__), #undef,
 * #include, #if / #elif / #ifdef / #ifndef / #else / #endif, #error.
 * #line.  #pragma / unknown: skipped.  # stringize / ## paste: expanded.
 *
 * Token-level: next() calls pp_directive() on TK_HASH, expands macros on
 * TK_IDENT.  Predefs: __STDC__, __LINE__, __FILE__, arch macros (see
 * pp_install_predefs / next() specials).  Map: docs/DIALECT.md.
 */

#define PP_MAX_DEFS  8192   /* was 2048 */  /* doom's header web defines >512 macros */
#define PP_MAX_IF    16

static char *pp_dname[PP_MAX_DEFS];
static int   pp_dval[PP_MAX_DEFS];
static char *pp_dbody[PP_MAX_DEFS];   /* body text (strdup'd), NULL = int-only */
static int   pp_dnpar[PP_MAX_DEFS];   /* param count, -1 = object-like */
static char *pp_dparm[PP_MAX_DEFS];   /* packed param names "a\0b\0" (strdup'd) */
static int   pp_dvar[PP_MAX_DEFS];    /* 1 = variadic macro (.../__VA_ARGS__) */
static char *pp_dsig[PP_MAX_DEFS];   /* the definition as compared on redefinition (strdup'd); NULL = none */
/* A hash index over the table: SQLite defines 2,800 macros, and a linear
 * scan with strcmp on every identifier token was 93% of its compile
 * (the DBT's PC probe: strcmp 62%, pp_find 31%).  Chains run newest
 * first, so a redefinition still wins as the backward scan did. */
#define PP_HASH_SZ 16384
static int   pp_hhead[PP_HASH_SZ];
static int   pp_dnext[PP_MAX_DEFS];
static int   pp_hash_ready;

static int pp_hash(char *name) {
    unsigned int h;
    int i;
    h = 5381;
    i = 0;
    while (name[i] != 0) {
        h = h * 33 + (unsigned char)name[i];
        i = i + 1;
    }
    return (int)(h & (PP_HASH_SZ - 1));
}

static void pp_hash_reset(void) {
    int i;
    i = 0;
    while (i < PP_HASH_SZ) { pp_hhead[i] = -1; i = i + 1; }
    pp_hash_ready = 1;
}
static int   pp_ndefs;

/* The presumed file name, for __FILE__ and diagnostics (6.10.8.1): the
 * file the lexer is in -- an included file's own path while its text is
 * read -- or what #line last named.  lex_line is the presumed line in
 * it.  The splice marks each included file (#s12cc_pushdir); its handler
 * saves the includer's name and line here and pp_popdir puts them back.
 * They were the top-level file's, and lex_line counted the whole
 * translation unit: __LINE__ after #include <stdio.h> was in the 200s. */
#define PP_FILE_LEN 512
static char pp_curfile[PP_FILE_LEN];

/* Where a diagnostic is: "file:line: ", the presumed name and line, as
 * clang and gcc write it (it was "s12cc:" and the line counted through
 * every included header). */
/* The presumed file names, interned: a node records the index of the
 * one it was read in (Node.file), so a diagnostic after parsing can name
 * it.  pp_file_note runs wherever pp_curfile changes. */
#define PP_MAX_FILES 1024
static char *pp_files[PP_MAX_FILES];
static int pp_nfiles;
static int pp_file_id;

static void pp_file_note(void) {
    int i;
    i = 0;
    while (i < pp_nfiles) {
        if (strcmp(pp_files[i], pp_curfile) == 0) { pp_file_id = i; return; }
        i = i + 1;
    }
    if (pp_nfiles >= PP_MAX_FILES) { pp_file_id = 0; return; }
    pp_files[pp_nfiles] = strdup(pp_curfile);
    pp_file_id = pp_nfiles;
    pp_nfiles = pp_nfiles + 1;
}

/* "file:line: " for a place recorded earlier. */
static void pp_where_at(int file, int line) {
    if (file >= 0 && file < pp_nfiles && pp_files[file][0] != 0) fdputs(pp_files[file], 2);
    else fdputs("s12cc", 2);
    fdputc(58, 2);
    fdputuint(2, line);
    fdputs(": ", 2);
}

static void pp_where(void) {
    fdputs(pp_curfile[0] != 0 ? pp_curfile : "s12cc", 2);
    fdputc(58, 2);      /* ':' */
    fdputuint(2, lex_line);
    fdputs(": ", 2);
}

/* A constraint or syntax violation in a directive or a macro use: the
 * program is not valid C, and stops here (C11 5.1.1.3). */
static void pp_error(char *msg) {
    pp_where();
    fdputs("error: ", 2);
    fdputs(msg, 2);
    fdputc(10, 2);
    exit(1);
}

#define PP_EXP_SZ 8192
static char pp_exp[PP_EXP_SZ];

/* Static buffers for pp_define — kept out of stack to avoid
   exceeding the 12-bit addi immediate limit in tree-walk codegen */
#define PP_DEF_BODY_SZ 4096
static char pp_def_body[PP_DEF_BODY_SZ];
static char pp_def_parms[1024];

static int pp_skip;                /* 1 = skipping tokens (ifdef false branch) */
static int pp_stk[PP_MAX_IF];     /* ifdef nesting: was-skipping before this level */
static int pp_dep;                 /* ifdef nesting depth */
static int pp_elsed[PP_MAX_IF];   /* 1 once this level's #else is seen */
static int pp_taken[PP_MAX_IF];   /* 1 if any branch taken at this depth */

static char pp_sdir[256];         /* source directory prefix for includes */
/* Directory stack of the files currently being spliced.  A quoted #include
   resolves against the directory of the file that contains it (C's rule),
   not the top-level source's.  The splice is textual, so that context would
   otherwise be lost: pp_splice_file brackets each spliced file with
   #s12cc_pushdir / #s12cc_popdir marker lines that maintain this stack as
   the lexer reaches them -- the same idea as cpp's "# N file" lines. */
#define PP_MAX_INCDEPTH 16
static char pp_incdir[PP_MAX_INCDEPTH * 256];
static int pp_incdepth;
static char pp_incfile[PP_MAX_INCDEPTH * PP_FILE_LEN];   /* the includer's name ... */
static int pp_incif[PP_MAX_INCDEPTH];    /* pp_dep when the file began */
static int pp_incline[PP_MAX_INCDEPTH];                 /* ... and its #include's line */
#define PP_MAX_IDIRS 16
#define PP_IDIR_LEN  256
/* Flat layout (stage06's parser accepts only literal-int array dimensions
   and rejects 2-D global arrays): entry i occupies
   pp_idir[i*PP_IDIR_LEN .. i*PP_IDIR_LEN+PP_IDIR_LEN-1].
   Size is PP_MAX_IDIRS * PP_IDIR_LEN = 16 * 256 = 4096. */
static char pp_idir[4096];
static int pp_nidirs;

/* --- Text helpers (read from lex_src[lex_pos]) --- */

static void pp_skip_ws(void) {
    /* Space, tab, comments, and line splices.  #if walks lex_src as
     * raw bytes, so a comment left in place is a token: 1 then a block
     * comment then && 0 was 1, and defined(A) then a comment then &&
     * defined(B) dropped the && (GitHub issue 39).  Consuming a splice
     * without bumping lex_line is the documented few-hundred-line drift
     * on continued directives. */
    /* Bound by the buffer, not lex_len: #if text-macro expansions park
     * past the file (GitHub issue 45). */
    while (lex_pos < LEX_SRC_SZ && lex_src[lex_pos] != 0) {
        if (lex_src[lex_pos] == 32 || lex_src[lex_pos] == 9) {
            lex_pos = lex_pos + 1;
        } else if (lex_src[lex_pos] == 92 && lex_src[lex_pos + 1] == 10) {
            lex_pos = lex_pos + 2;
            lex_line = lex_line + 1;
        } else if (lex_src[lex_pos] == 92 && lex_src[lex_pos + 1] == 13 &&
                   lex_src[lex_pos + 2] == 10) {
            lex_pos = lex_pos + 3;
            lex_line = lex_line + 1;
        } else if (lex_src[lex_pos] == 47 && lex_pos + 1 < LEX_SRC_SZ &&
                   lex_src[lex_pos + 1] == 42) {
            lex_pos = lex_pos + 2;
            while (lex_pos + 1 < LEX_SRC_SZ &&
                   !(lex_src[lex_pos] == 42 && lex_src[lex_pos + 1] == 47)) {
                if (lex_src[lex_pos] == 10) lex_line = lex_line + 1;
                lex_pos = lex_pos + 1;
            }
            if (lex_pos + 1 < LEX_SRC_SZ) lex_pos = lex_pos + 2;
        } else if (lex_src[lex_pos] == 47 && lex_pos + 1 < LEX_SRC_SZ &&
                   lex_src[lex_pos + 1] == 47) {
            lex_pos = lex_pos + 2;
            while (lex_pos < LEX_SRC_SZ && lex_src[lex_pos] != 10) {
                if (lex_src[lex_pos] == 92 && lex_src[lex_pos + 1] == 10) {
                    lex_pos = lex_pos + 2;
                    lex_line = lex_line + 1;
                    continue;
                }
                if (lex_src[lex_pos] == 92 && lex_src[lex_pos + 1] == 13 &&
                    lex_src[lex_pos + 2] == 10) {
                    lex_pos = lex_pos + 3;
                    lex_line = lex_line + 1;
                    continue;
                }
                lex_pos = lex_pos + 1;
            }
        } else {
            break;
        }
    }
}

static void pp_skip_line(void) {
    /* To the end of the logical line: a backslash before the newline
     * continues it (SQLite's `# error "...\` messages inside a skipped
     * branch -- stopping at the physical newline left the message's tail
     * as text, and its closing quote opened a string that swallowed the
     * #endif). The final newline is left for the caller, as before. */
    while (lex_pos < lex_len) {
        if (lex_src[lex_pos] == 10) {
            if (lex_pos > 0 && lex_src[lex_pos - 1] == 92) {
                lex_pos = lex_pos + 1;
                lex_line = lex_line + 1;
                continue;
            }
            if (lex_pos > 1 && lex_src[lex_pos - 1] == 13 && lex_src[lex_pos - 2] == 92) {
                lex_pos = lex_pos + 1;
                lex_line = lex_line + 1;
                continue;
            }
            return;
        }
        /* A block comment on the directive's tail may run past the newline
         * (SQLite's `#define PGHDR_NEED_SYNC 0x008 /* ...` continues on
         * the next line); it is consumed whole, as the standard's
         * comment replacement would have done before the directive. */
        if (lex_src[lex_pos] == 47 && lex_pos + 1 < lex_len && lex_src[lex_pos + 1] == 42) {
            lex_pos = lex_pos + 2;
            while (lex_pos + 1 < lex_len &&
                   !(lex_src[lex_pos] == 42 && lex_src[lex_pos + 1] == 47)) {
                if (lex_src[lex_pos] == 10) lex_line = lex_line + 1;
                lex_pos = lex_pos + 1;
            }
            if (lex_pos + 1 < lex_len) lex_pos = lex_pos + 2;
            continue;
        }
        lex_pos = lex_pos + 1;
    }
}

/* Raw-byte scan from lex_pos to the next preprocessor directive (`#`),
 * or to EOF.  Used while pp_skip is set so we don't full-tokenize the
 * skipped region — that path goes through the Ragel lexer, which has
 * no rule for `\` (line-continuation) outside string/char literals and
 * loops forever when a skipped block contains a multi-line `#define`
 * body.  This scanner just respects strings, char literals, and
 * comments while advancing lex_pos. */
static void pp_fast_skip_to_directive(void) {
    int c;
    while (lex_pos < lex_len) {
        c = lex_src[lex_pos] & 255;
        if (c == 32 || c == 9 || c == 11 || c == 12 || c == 13) {
            lex_pos = lex_pos + 1;
            continue;
        }
        if (c == 10) {
            lex_pos = lex_pos + 1;
            lex_line = lex_line + 1;
            continue;
        }
        /* Backslash-newline: line continuation — consume both. */
        if (c == 92 && lex_pos + 1 < lex_len && lex_src[lex_pos + 1] == 10) {
            lex_pos = lex_pos + 2;
            lex_line = lex_line + 1;
            continue;
        }
        /* Line comment */
        if (c == 47 && lex_pos + 1 < lex_len && lex_src[lex_pos + 1] == 47) {
            while (lex_pos < lex_len && lex_src[lex_pos] != 10) {
                lex_pos = lex_pos + 1;
            }
            continue;
        }
        /* Block comment */
        if (c == 47 && lex_pos + 1 < lex_len && lex_src[lex_pos + 1] == 42) {
            lex_pos = lex_pos + 2;
            while (lex_pos + 1 < lex_len &&
                   !(lex_src[lex_pos] == 42 && lex_src[lex_pos + 1] == 47)) {
                if (lex_src[lex_pos] == 10) lex_line = lex_line + 1;
                lex_pos = lex_pos + 1;
            }
            if (lex_pos + 1 < lex_len) lex_pos = lex_pos + 2;
            continue;
        }
        /* String literal: consume until matching `"`, honoring escapes. */
        if (c == 34) {
            lex_pos = lex_pos + 1;
            while (lex_pos < lex_len && lex_src[lex_pos] != 34) {
                if (lex_src[lex_pos] == 92 && lex_pos + 1 < lex_len) {
                    if (lex_src[lex_pos + 1] == 10) lex_line = lex_line + 1;
                    lex_pos = lex_pos + 2;
                    continue;
                }
                if (lex_src[lex_pos] == 10) lex_line = lex_line + 1;
                lex_pos = lex_pos + 1;
            }
            if (lex_pos < lex_len) lex_pos = lex_pos + 1;
            continue;
        }
        /* Char literal */
        if (c == 39) {
            lex_pos = lex_pos + 1;
            while (lex_pos < lex_len && lex_src[lex_pos] != 39) {
                if (lex_src[lex_pos] == 92 && lex_pos + 1 < lex_len) {
                    lex_pos = lex_pos + 2;
                    continue;
                }
                lex_pos = lex_pos + 1;
            }
            if (lex_pos < lex_len) lex_pos = lex_pos + 1;
            continue;
        }
        /* Found a directive marker. */
        if (c == 35) return;
        /* Anything else: skip the byte. */
        lex_pos = lex_pos + 1;
    }
}

static void pp_add_idir(char *path) {
    int base;
    int j;

    if (pp_nidirs >= PP_MAX_IDIRS) return;
    base = pp_nidirs * PP_IDIR_LEN;
    j = 0;
    while (path[j] != 0 && j < PP_IDIR_LEN - 2) {
        pp_idir[base + j] = path[j];
        j = j + 1;
    }
    if (j > 0 && pp_idir[base + j - 1] != 47) {
        pp_idir[base + j] = 47;
        j = j + 1;
    }
    pp_idir[base + j] = 0;
    pp_nidirs = pp_nidirs + 1;
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

/* 1 = the value the #if evaluator last produced is uintmax_t (C11
 * 6.10.1p4): each level sets it for its result, and a binary operator
 * reads its left operand's before evaluating the right. */
static int pp_ev_u;

/* #if arithmetic is intmax_t (GitHub issue 60).  Accumulate in
 * unsigned long long so 2147483648 and 0x80000000 do not wrap to
 * a negative int; the signed result is what C11 6.10.1 uses. */
static long long pp_read_int(void) {
    unsigned long long uval;
    int neg;
    int c;
    uval = 0;
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
                uval = uval * 16 + (unsigned long long)(c - 97 + 10);
            } else if (c >= 65) {
                uval = uval * 16 + (unsigned long long)(c - 65 + 10);
            } else {
                uval = uval * 16 + (unsigned long long)(c - 48);
            }
            lex_pos = lex_pos + 1;
            c = lex_src[lex_pos];
        }
    } else if (c == 48 && lex_src[lex_pos + 1] >= 48 && lex_src[lex_pos + 1] <= 55) {
        /* Octal: 0...  A numeric #define body is folded here and re-emitted
         * as its value, so `#define S_IFREG 0100000` in sys/stat.h came
         * back as decimal 100000 and S_ISREG() was false for every regular
         * file (regal's template loader found no templates). */
        lex_pos = lex_pos + 1;
        c = lex_src[lex_pos];
        while (c >= 48 && c <= 55) {
            uval = uval * 8 + (unsigned long long)(c - 48);
            lex_pos = lex_pos + 1;
            c = lex_src[lex_pos];
        }
    } else {
        /* Decimal */
        while (c >= 48 && c <= 57) {
            uval = uval * 10 + (unsigned long long)(c - 48);
            lex_pos = lex_pos + 1;
            c = lex_src[lex_pos];
        }
    }
    /* Skip U/L suffixes; a u, or a value past intmax_t, is uintmax_t */
    pp_ev_u = uval > 9223372036854775807ULL;
    c = lex_src[lex_pos];
    while (c == 85 || c == 117 || c == 76 || c == 108) {
        if (c == 85 || c == 117) pp_ev_u = 1;
        lex_pos = lex_pos + 1;
        c = lex_src[lex_pos];
    }
    if (neg) return -(long long)uval;
    return (long long)uval;
}

static void pp_body_putc(char *buf, int *bi, int c) {
    if (*bi >= PP_DEF_BODY_SZ - 1) {
        fdputs("s12cc: #define body too long\n", 2);
        exit(1);
    }
    buf[*bi] = c;
    *bi = *bi + 1;
}

static int pp_read_body_text(char *buf) {
    int bi;
    int c;
    int in_str;
    int in_chr;
    int esc;

    bi = 0;
    in_str = 0;
    in_chr = 0;
    esc = 0;
    while (lex_pos < lex_len) {
        /* Line splice: backslash + newline (or \r\n) — consume both,
           bump lex_line so error messages stay correct. */
        if (lex_src[lex_pos] == 92 && lex_src[lex_pos + 1] == 10) {
            lex_pos = lex_pos + 2;
            lex_line = lex_line + 1;
            continue;
        }
        if (lex_src[lex_pos] == 92 && lex_src[lex_pos + 1] == 13 &&
            lex_src[lex_pos + 2] == 10) {
            lex_pos = lex_pos + 3;
            lex_line = lex_line + 1;
            continue;
        }
        /* Real end of macro body: unspliced newline. */
        if (lex_src[lex_pos] == 10 || lex_src[lex_pos] == 13) break;
        c = lex_src[lex_pos];
        if (!in_str && !in_chr && c == 47) {
            if (lex_src[lex_pos + 1] == 47) break;
            if (lex_src[lex_pos + 1] == 42) {
                /* A block comment is a space in the body, and may run past
                 * the newline (SQLite: `#define PGHDR_NEED_SYNC 0x008 /* ...`
                 * continued on the next line).  Stopping here as a trailing
                 * comment cut the body short and left the comment's second
                 * line as source. */
                lex_pos = lex_pos + 2;
                while (lex_pos + 1 < lex_len &&
                       !(lex_src[lex_pos] == 42 && lex_src[lex_pos + 1] == 47)) {
                    if (lex_src[lex_pos] == 10) lex_line = lex_line + 1;
                    lex_pos = lex_pos + 1;
                }
                lex_pos = lex_pos + 2;
                pp_body_putc(buf, &bi, 32);
                continue;
            }
        }
        pp_body_putc(buf, &bi, c);
        lex_pos = lex_pos + 1;
        if (esc) {
            esc = 0;
        } else if (c == 92) {
            esc = 1;
        } else if (!in_chr && c == 34) {
            in_str = !in_str;
        } else if (!in_str && c == 39) {
            in_chr = !in_chr;
        }
    }
    while (bi > 0 && (buf[bi - 1] == 32 || buf[bi - 1] == 9)) {
        bi = bi - 1;
    }
    buf[bi] = 0;
    return bi;
}

/* --- Define table --- */

static int pp_find(char *name) {
    int i;
    if (!pp_hash_ready) pp_hash_reset();
    i = pp_hhead[pp_hash(name)];
    while (i >= 0) {
        if (pp_dname[i] != 0 && strcmp(name, pp_dname[i]) == 0) return i;
        i = pp_dnext[i];
    }
    return -1;
}

/* #ifdef, #ifndef and defined(): a macro, or __has_include itself, so
 * that `#if defined(__has_include)` guards its use. */
static int pp_is_defined(char *name) {
    if (pp_find(name) >= 0) return 1;
    return strcmp(name, "__has_include") == 0;
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
        fdputs("s12cc: too many #defines\n", 2);
        exit(1);
    }
    pp_dname[pp_ndefs] = strdup(name);
    pp_dval[pp_ndefs] = val;
    pp_dbody[pp_ndefs] = 0;
    pp_dnpar[pp_ndefs] = -1;
    pp_dparm[pp_ndefs] = 0;
    pp_dvar[pp_ndefs] = 0;
    pp_dsig[pp_ndefs] = 0;
    if (!pp_hash_ready) pp_hash_reset();
    i = pp_hash(name);
    pp_dnext[pp_ndefs] = pp_hhead[i];
    pp_hhead[i] = pp_ndefs;
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
    /* Every caller stands at a token boundary, so the scanner restarts
     * with no token in progress.  A stale token start from the scan that
     * found the macro name made it emit the bytes between that start and
     * the resumed position as an identifier ("SM" from "SMALL" once the
     * expansion was laid down backwards). */
    lex_rcs = c_lexer_start;
    lex_rts = 0;
    lex_rte = 0;
    lex_ract = 0;
}

/* --- Macro expansion --- */

/* C99 6.10.3.4p2: while a macro's replacement is being rescanned, that
 * macro's name is not expanded again.  Each expansion records the buffer
 * position where its replacement ends; a name whose macro is on the stack
 * stays an identifier until the scan passes that end.  Without this,
 * SQLite's `#define vfsList GLOBAL(sqlite3_vfs *, vfsList)` expanded a
 * million times and the compile looked like a hang. */
/* The parser peeks past a '(' to tell a cast from a grouping paren, saving
 * the scanner and restoring it: a restore re-lexes from the saved pointer,
 * so while a peek is live an expansion must replace the name in place (the
 * shift) rather than run backwards into the prefix. */
static int pp_peek_depth;

#define PP_DIS_MAX 256
static int pp_dis_di[PP_DIS_MAX];
static int pp_dis_end[PP_DIS_MAX];
static int pp_ndis;
/* Entries below pp_dis_base belong to the text around a call whose
 * argument is being pre-expanded (pp_preexpand): they hold for the
 * whole argument and are not popped by positions in it. */
static int pp_dis_base;
/* Where the replacement just laid down by pp_expand_func/obj ends. */
static int pp_xend;

/* Each expansion's macro stays disabled until the scan passes `end`.
 * There used to be a second list, the argument regions inside each
 * expansion, exempt from it: arguments were substituted raw and
 * expanded only on the rescan, so a macro's own name in its argument
 * (ROUND8(SZ_VDBECURSOR(n))) had to stay live there.  Arguments are now
 * expanded before substitution (pp_preexpand), as 6.10.3.1 has it, and
 * the exemption is gone: in m(m), the m substituted for a(w) is met
 * during m's own rescan and is not replaced again (6.10.3.4p2) -- it
 * recursed until the stack ran out. */
static void pp_dis_push(int di, int end) {
    if (pp_ndis >= PP_DIS_MAX) {
        /* Dropping the entry and still emitting the body is the vfsList
         * infinite-expansion class, now silent (GitHub issue 48). */
        fdputs("s12cc: too many nested macro expansions\n", 2);
        exit(1);
    }
    pp_dis_di[pp_ndis] = di;
    pp_dis_end[pp_ndis] = end;
    pp_ndis = pp_ndis + 1;
}

/* Entries that end at or before `at` are finished. */
static void pp_dis_pop(int at) {
    while (pp_ndis > pp_dis_base && pp_dis_end[pp_ndis - 1] <= at) pp_ndis = pp_ndis - 1;
}

/* The text after `ins` moved by delta: keep the ends true.  A caller
 * replacing text passes its last replaced position, so an expansion
 * whose last token was the replaced call or name moves with it -- with
 * `end > end of the call` the enclosing COUNT in COUNT(a) stayed
 * disabled past its shrunken text, and the next COUNT(a, b) with it. */
static void pp_dis_shift(int ins, int delta) {
    int k;
    k = pp_dis_base;
    while (k < pp_ndis) {
        if (pp_dis_end[k] > ins) pp_dis_end[k] = pp_dis_end[k] + delta;
        k = k + 1;
    }
}

static int pp_disabled(int di, int at) {
    int k;
    pp_dis_pop(at);
    k = 0;
    while (k < pp_ndis) {
        if (pp_dis_di[k] == di) return 1;
        k = k + 1;
    }
    return 0;
}

/* The newlines and comments of a macro invocation.  Inside the call a
 * newline is white space (6.10.3p10), and a comment already was
 * (5.1.1.2p3); both become spaces here, in lex_src[from..to), before
 * the arguments are substituted.  An argument that broke across a line
 * and appeared twice in the body brought its newline twice (SQLite's
 * sqlite3ResolveNotValid, three of them: __LINE__ ran 3 ahead); #x
 * spelled a comment into its string; and a // comment, its newline
 * gone, swallowed the rest of the expansion.  Returns how many newlines
 * there were, for pp_keep_newlines. */
static int pp_call_newlines(int from, int to) {
    int k;
    int n;
    int q;
    n = 0;
    k = from;
    while (k < to) {
        q = lex_src[k];
        if (q == 34 || q == 39) {
            k = k + 1;
            while (k < to && lex_src[k] != q) {
                if (lex_src[k] == 92) k = k + 1;
                k = k + 1;
            }
            k = k + 1;
            continue;
        }
        if (q == 47 && k + 1 < to && lex_src[k + 1] == 42) {
            lex_src[k] = 32;
            lex_src[k + 1] = 32;
            k = k + 2;
            while (k < to && !(lex_src[k] == 42 && k + 1 < to && lex_src[k + 1] == 47)) {
                if (lex_src[k] == 10) n = n + 1;
                lex_src[k] = 32;
                k = k + 1;
            }
            if (k < to) {
                lex_src[k] = 32;
                lex_src[k + 1] = 32;
                k = k + 2;
            }
            continue;
        }
        if (q == 47 && k + 1 < to && lex_src[k + 1] == 47) {
            while (k < to && lex_src[k] != 10) {
                lex_src[k] = 32;
                k = k + 1;
            }
            continue;
        }
        if (q == 10) {
            n = n + 1;
            lex_src[k] = 32;
            if (k > from && lex_src[k - 1] == 92) lex_src[k - 1] = 32;
        }
        k = k + 1;
    }
    return n;
}

/* ... and appended, n of them, to the expansion pp_exp[0..len), so the
 * lexer still counts them -- on every scan of the text, a rewound one
 * too.  Dropped, each two-line assert( ... ) in SQLite cost __LINE__
 * one: 6 by sqlite3_status64, hundreds by the end.  Returns the new
 * length. */
static int pp_keep_newlines(int n, int len) {
    while (n > 0 && len < PP_EXP_SZ - 1) {
        pp_exp[len] = 10;
        len = len + 1;
        n = n - 1;
    }
    return len;
}

static int pp_exp_putc(int len, int c) {
    if (len < PP_EXP_SZ - 1) {
        pp_exp[len] = c;
        len = len + 1;
    }
    return len;
}

/* #param: lex_src[from..to) as a string literal, appended to pp_exp at
 * len (6.10.3.2p2).  A run of white space is one space, and leading and
 * trailing runs vanish -- an assert whose expression spanned two lines
 * put a raw newline inside the string.  Inside a string or character
 * literal the text is kept as it is, with a \ before each " and \;
 * outside one a \ is copied bare: S(a\n) is "a\n", which the literal
 * then reads as a newline, as clang's does.  Returns the new length. */
static int pp_stringize(int from, int to, int len) {
    int k;
    int ch;
    int q;
    int pend_sp;    /* whitespace seen, not yet emitted */
    int any;        /* a non-blank character has been emitted */
    pend_sp = 0;
    any = 0;
    q = 0;
    len = pp_exp_putc(len, 34);
    k = from;
    while (k < to) {
        ch = lex_src[k];
        k = k + 1;
        if (q == 0 && ch == 1) continue;     /* paint (pp_rescan_region) */
        if (q == 0 && (ch == 32 || ch == 9 || ch == 10 || ch == 13)) {
            pend_sp = 1;
            continue;
        }
        if (pend_sp && any) len = pp_exp_putc(len, 32);
        pend_sp = 0;
        any = 1;
        if (q == 0) {
            if (ch == 34 || ch == 39) q = ch;
        } else if (ch == 92 && k < to) {
            len = pp_exp_putc(len, 92);
            len = pp_exp_putc(len, 92);
            ch = lex_src[k];
            k = k + 1;
        } else if (ch == q) {
            q = -1;
        }
        if (q != 0 && (ch == 34 || ch == 92)) len = pp_exp_putc(len, 92);
        len = pp_exp_putc(len, ch);
        if (q == -1) q = 0;
    }
    return pp_exp_putc(len, 34);
}

#define PP_MAX_ARGS 32
#define PP_PRE_SZ 1048576
/* Pre-expanded arguments, stacked: each call takes what it needs above
 * pp_pre_top and gives it back when its body is laid down. */
static char pp_pre[PP_PRE_SZ];
static int pp_pre_top;
static int pp_pre_n;            /* length of the last pp_preexpand result */
static char pp_iname[256];
#define PP_SPELL_SZ 1100
static char pp_spell[PP_SPELL_SZ];

static int pp_expand_func(int di);
static void pp_expand_obj(int di);

static int pp_is_idstart(int c) {
    return (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c == 95;
}

static int pp_is_idch(int c) {
    return pp_is_idstart(c) || (c >= 48 && c <= 57);
}

/* The identifier at lex_src[k] into pp_iname; returns where it ends. */
static int pp_read_ident_at(int k) {
    int n;
    n = 0;
    while (pp_is_idch(lex_src[k])) {
        if (n < 255) { pp_iname[n] = lex_src[k]; n = n + 1; }
        k = k + 1;
    }
    pp_iname[n] = 0;
    return k;
}

/* Past the literal, or the pp-number, that starts at lex_src[k]. */
static int pp_skip_literal(int k, int to) {
    int q;
    q = lex_src[k];
    k = k + 1;
    while (k < to && lex_src[k] != q) {
        if (lex_src[k] == 92) k = k + 1;
        k = k + 1;
    }
    return k + 1;
}

static int pp_skip_ppnum(int k, int to) {
    int c;
    while (k < to) {
        c = lex_src[k];
        if ((c == 43 || c == 45) && (lex_src[k - 1] == 101 || lex_src[k - 1] == 69 ||
                                     lex_src[k - 1] == 112 || lex_src[k - 1] == 80)) {
            k = k + 1;
            continue;
        }
        if (!pp_is_idch(c) && c != 46) break;
        k = k + 1;
    }
    return k;
}

/* Would the two characters run together into one token?  A replacement
 * is spliced into text, and in pp_preexpand the result is kept as text:
 * x-NEG with NEG -1 must not become x--1. */
static int pp_glues(int a, int b) {
    if ((pp_is_idch(a) || a == 46) && (pp_is_idch(b) || b == 46 || b == 1 || b == 34 || b == 39)) return 1;
    if (a == 0 || b == 0) return 0;
    return strchr("+-*/%&|^<>=!#:.", a) != 0 && strchr("+-*/%&|^<>=!#:.", b) != 0;
}

/* lex_src[at..at+dellen) becomes ins[0..inslen), the tail moving. */
static void pp_splice(int at, int dellen, char *ins, int inslen) {
    int delta;
    int tail;
    int i;
    delta = inslen - dellen;
    tail = lex_len - (at + dellen);
    if (lex_len + delta >= LEX_SRC_SZ - 1) {
        fdputs("s12cc: macro expansion overflows the source buffer\n", 2);
        exit(1);
    }
    if (delta > 0) {
        i = tail - 1;
        while (i >= 0) {
            lex_src[at + inslen + i] = lex_src[at + dellen + i];
            i = i - 1;
        }
    } else if (delta < 0) {
        i = 0;
        while (i < tail) {
            lex_src[at + inslen + i] = lex_src[at + dellen + i];
            i = i + 1;
        }
    }
    i = 0;
    while (i < inslen) {
        lex_src[at + i] = ins[i];
        i = i + 1;
    }
    lex_len = lex_len + delta;
    lex_src[lex_len] = 0;
    if (dellen > 0) pp_dis_shift(at + dellen - 1, delta);
    else pp_dis_shift(at, delta);
}

/* A space where lex_src[at-1] and lex_src[at] would glue. */
static void pp_unglue_at(int at) {
    if (pp_glues(lex_src[at - 1], lex_src[at])) pp_splice(at, 0, " ", 1);
}

static int pp_spell_int(int v) {
    unsigned u;
    int n;
    int i;
    int j;
    char t;
    n = 0;
    u = (unsigned)v;
    if (v < 0) {
        pp_spell[0] = 45;
        n = 1;
        u = 0u - u;
    }
    i = n;
    do {
        pp_spell[n] = 48 + (int)(u % 10u);
        n = n + 1;
        u = u / 10u;
    } while (u != 0u);
    j = n - 1;
    while (i < j) {
        t = pp_spell[i];
        pp_spell[i] = pp_spell[j];
        pp_spell[j] = t;
        i = i + 1;
        j = j - 1;
    }
    return n;
}

static int pp_spell_file(void) {
    int n;
    int k;
    n = 0;
    pp_spell[n] = 34;
    n = n + 1;
    k = 0;
    while (pp_curfile[k] != 0 && n < PP_SPELL_SZ - 3) {
        if (pp_curfile[k] == 34 || pp_curfile[k] == 92) {
            pp_spell[n] = 92;
            n = n + 1;
        }
        pp_spell[n] = pp_curfile[k];
        n = n + 1;
        k = k + 1;
    }
    pp_spell[n] = 34;
    return n + 1;
}

/* Does lex_src[from..to) hold a name that would expand?  Most
 * arguments do not, and are their own expansion. */
static int pp_has_macro(int from, int to) {
    int k;
    int c;
    k = from;
    while (k < to) {
        c = lex_src[k];
        if (c == 34 || c == 39) {
            k = pp_skip_literal(k, to);
        } else if (c >= 48 && c <= 57) {
            k = pp_skip_ppnum(k, to);
        } else if (c == 1) {
            k = pp_read_ident_at(k + 1);
        } else if (pp_is_idstart(c)) {
            k = pp_read_ident_at(k);
            if (pp_find(pp_iname) >= 0) return 1;
            if (strcmp(pp_iname, "__LINE__") == 0 || strcmp(pp_iname, "__FILE__") == 0) return 1;
        } else {
            k = k + 1;
        }
    }
    return 0;
}

/* Expand every macro in lex_src[r..lex_len), in place, rescanning each
 * replacement.  A name that is disabled here is painted -- a 0x01 byte
 * goes in front of it, which the lexer reads as "an identifier never to
 * be replaced" (6.10.3.4p2: it stays unreplaced even where it would
 * otherwise be). */
static void pp_rescan_region(int r) {
    int k;
    int s;
    int e;
    int c;
    int di;
    int n;
    k = r;
    while (k < lex_len) {
        c = lex_src[k];
        if (c == 34 || c == 39) { k = pp_skip_literal(k, lex_len); continue; }
        if (c >= 48 && c <= 57) { k = pp_skip_ppnum(k, lex_len); continue; }
        if (c == 1) { k = pp_read_ident_at(k + 1); continue; }
        if (!pp_is_idstart(c)) { k = k + 1; continue; }
        s = k;
        e = pp_read_ident_at(k);
        if (strcmp(pp_iname, "__LINE__") == 0 || strcmp(pp_iname, "__FILE__") == 0) {
            if (pp_iname[2] == 76) n = pp_spell_int(lex_line);
            else n = pp_spell_file();
            pp_splice(s, e - s, pp_spell, n);
            pp_xend = s + n;
        } else {
            di = pp_find(pp_iname);
            if (di < 0) { k = e; continue; }
            if (pp_disabled(di, s)) {
                pp_spell[0] = 1;        /* stage07 reads "\001" as 0 */
                pp_splice(s, 0, pp_spell, 1);
                k = e + 1;
                continue;
            }
            if (pp_dnpar[di] >= 0) {
                lex_rp = lex_src + e;
                if (!pp_expand_func(di)) { k = e; continue; }
                /* The replacement went in at the '('; the name is still
                 * in front of it. */
                n = lex_pos - s;
                pp_splice(s, n, pp_spell, 0);
                pp_xend = pp_xend - n;
            } else if (pp_dbody[di] != 0) {
                lex_rp = lex_src + e;
                lex_slen = e - s;
                pp_expand_obj(di);
            } else {
                n = pp_spell_int(pp_dval[di]);
                pp_splice(s, e - s, pp_spell, n);
                pp_xend = s + n;
            }
        }
        if (pp_xend < lex_len) pp_unglue_at(pp_xend);
        if (s > r) pp_unglue_at(s);
        k = s;
    }
}

/* Argument pre-expansion (6.10.3.1p1): lex_src[from..to), with every
 * macro in it replaced as if it were the rest of the file, onto the
 * pp_pre stack; returns the offset, the length in pp_pre_n.  The text
 * is copied past the end of the buffer and expanded there in place;
 * whatever is disabled around the call stays disabled for it.  Without
 * this, XSTR(VAL) gave "VAL", not "42", and an argument next to ## or
 * after # was the only kind there was. */
static int pp_preexpand(int from, int to) {
    int r;
    int n;
    int i;
    int off;
    int saved_len;
    int saved_pos;
    int saved_peek;
    int saved_base;
    saved_len = lex_len;
    saved_pos = lex_pos;
    saved_peek = pp_peek_depth;
    saved_base = pp_dis_base;
    pp_dis_pop(from);
    pp_dis_base = pp_ndis;
    n = to - from;
    r = lex_len + 2;
    if (r + n + 2 >= LEX_SRC_SZ) {
        fdputs("s12cc: macro argument overflows the source buffer\n", 2);
        exit(1);
    }
    i = 0;
    while (i < n) {
        lex_src[r + i] = lex_src[from + i];
        i = i + 1;
    }
    lex_len = r + n;
    lex_src[lex_len] = 0;
    pp_peek_depth = 1;          /* replace in place: the text is the result */
    pp_rescan_region(r);
    n = lex_len - r;
    if (pp_pre_top + n > PP_PRE_SZ) {
        fdputs("s12cc: macro arguments too large\n", 2);
        exit(1);
    }
    off = pp_pre_top;
    i = 0;
    while (i < n) {
        pp_pre[off + i] = lex_src[r + i];
        i = i + 1;
    }
    pp_pre_top = off + n;
    pp_pre_n = n;
    pp_ndis = pp_dis_base;
    pp_dis_base = saved_base;
    lex_len = saved_len;
    lex_pos = saved_pos;
    pp_peek_depth = saved_peek;
    return off;
}

/* Which parameter slot is the name in pp_iname: its index, npar for
 * __VA_ARGS__ of a variadic macro, or -1. */
static int pp_param_slot(int di) {
    char *dp;
    int pi;
    int k;
    dp = pp_dparm[di];
    if (dp != 0) {
        pi = 0;
        k = 0;
        while (pi < pp_dnpar[di]) {
            if (strcmp(pp_iname, dp + k) == 0) return pi;
            while (dp[k] != 0) k = k + 1;
            k = k + 1;
            pi = pi + 1;
        }
    }
    if (pp_dvar[di] && strcmp(pp_iname, "__VA_ARGS__") == 0) return pp_dnpar[di];
    return -1;
}

/* The body of function-like macro di into pp_exp, its parameters
 * replaced (6.10.3.1).  Slot i's argument is lex_src[rfrom[i]..rto[i]);
 * where pre_off[i] >= 0 its expansion is pp_pre[pre_off[i]..+pre_len[i]),
 * and that is what a parameter outside # and ## becomes.  need[i] is
 * set for each slot used so.  Returns the length. */
static int pp_subst(int di, int *rfrom, int *rto, int *pre_off, int *pre_len, int *need) {
    char *body;
    int len;
    int j;
    int k;
    int c;
    int q;
    int slot;
    int paste;      /* the last operator was ## */
    body = pp_dbody[di];
    len = 0;
    j = 0;
    paste = 0;
    while (body[j] != 0) {
        c = body[j];
        /* A string or character literal in the body is copied whole: a
         * parameter name inside it is text, not a parameter (C99
         * 6.10.3p10 substitutes preprocessing tokens, and a literal is
         * one token).  regal's TEST_EQ_STR(r, actual, expected, msg)
         * printed "(got '%s', expected '%s')" -- and `expected` inside
         * that string came out as the argument, "2025-10-15". */
        if (c == 34 || c == 39) {
            q = c;
            len = pp_exp_putc(len, c);
            j = j + 1;
            while (body[j] != 0 && body[j] != q) {
                if (body[j] == 92 && body[j + 1] != 0) {
                    len = pp_exp_putc(len, body[j]);
                    j = j + 1;
                }
                len = pp_exp_putc(len, body[j]);
                j = j + 1;
            }
            if (body[j] == q) {
                len = pp_exp_putc(len, q);
                j = j + 1;
            }
            paste = 0;
            continue;
        }

        /* Token paste: ## — drop operator and adjacent body whitespace so
         * the preceding and following tokens abut.  A pasted name is a
         * new token: any paint on the left one comes off. */
        if (c == 35 && body[j + 1] == 35) {  /* '#' '#' */
            j = j + 2;
            while (body[j] == 32 || body[j] == 9) j = j + 1;
            while (len > 0 && (pp_exp[len - 1] == 32 || pp_exp[len - 1] == 9)) len = len - 1;
            k = len;
            while (k > 0 && pp_is_idch(pp_exp[k - 1])) k = k - 1;
            if (k > 0 && pp_exp[k - 1] == 1) {
                while (k < len) {
                    pp_exp[k - 1] = pp_exp[k];
                    k = k + 1;
                }
                len = len - 1;
            }
            paste = 1;
            continue;
        }

        /* Stringize: # param  →  "argument text" with escapes */
        if (c == 35) {
            j = j + 1;
            while (body[j] == 32 || body[j] == 9) j = j + 1;
            k = 0;
            while (pp_is_idch(body[j])) {
                if (k < 255) { pp_iname[k] = body[j]; k = k + 1; }
                j = j + 1;
            }
            pp_iname[k] = 0;
            slot = -1;
            if (k > 0) slot = pp_param_slot(di);
            if (slot >= 0) len = pp_stringize(rfrom[slot], rto[slot], len);
            else len = pp_stringize(0, 0, len);
            paste = 0;
            continue;
        }

        if (pp_is_idstart(c)) {
            k = 0;
            while (pp_is_idch(body[j])) {
                if (k < 255) { pp_iname[k] = body[j]; k = k + 1; }
                j = j + 1;
            }
            pp_iname[k] = 0;
            slot = pp_param_slot(di);
            if (slot < 0) {
                k = 0;
                while (pp_iname[k] != 0) {
                    len = pp_exp_putc(len, pp_iname[k]);
                    k = k + 1;
                }
                paste = 0;
                continue;
            }
            k = j;
            while (body[k] == 32 || body[k] == 9) k = k + 1;
            if (!paste && !(body[k] == 35 && body[k + 1] == 35)) {
                need[slot] = 1;
                if (pre_off[slot] >= 0) {
                    k = 0;
                    while (k < pre_len[slot]) {
                        len = pp_exp_putc(len, pp_pre[pre_off[slot] + k]);
                        k = k + 1;
                    }
                    continue;
                }
            }
            k = rfrom[slot];
            if (paste && k < rto[slot] && lex_src[k] == 1) k = k + 1;
            while (k < rto[slot]) {
                len = pp_exp_putc(len, lex_src[k]);
                k = k + 1;
            }
            paste = 0;
            continue;
        }
        len = pp_exp_putc(len, c);
        if (c != 32 && c != 9) paste = 0;
        j = j + 1;
    }
    return len;
}

static int pp_expand_func(int di) {
    int call_nl;
    int pos;
    int ins_pos;
    int depth;
    int nargs;
    int arg_start[PP_MAX_ARGS];
    int arg_len[PP_MAX_ARGS];
    int rfrom[PP_MAX_ARGS + 1];
    int rto[PP_MAX_ARGS + 1];
    int pre_off[PP_MAX_ARGS + 1];
    int pre_len[PP_MAX_ARGS + 1];
    int need[PP_MAX_ARGS + 1];
    int nslot;
    int base;
    int again;
    int exp_len;
    int c;
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
            /* String and character literals, and comments, are opaque to
             * the paren count (C99 6.10.3p11 collects arguments from
             * preprocessing tokens).  Counting the '(' in SQLite's
             * assert( z[0]=='(' ) left the scan unbalanced to the end of
             * the 9MB buffer, hundreds of times over; and a ')' inside a
             * literal ended an argument early and swallowed real code. */
            if (c == 34 || c == 39) {
                int q;
                q = c;
                pos = pos + 1;
                while (lex_src[pos] != 0 && lex_src[pos] != q) {
                    if (lex_src[pos] == 92 && lex_src[pos + 1] != 0) pos = pos + 1;
                    pos = pos + 1;
                }
                if (lex_src[pos] == q) pos = pos + 1;
                continue;
            }
            if (c == 47 && lex_src[pos + 1] == 42) {
                pos = pos + 2;
                while (lex_src[pos] != 0 && !(lex_src[pos] == 42 && lex_src[pos + 1] == 47)) pos = pos + 1;
                if (lex_src[pos] != 0) pos = pos + 2;
                continue;
            }
            if (c == 47 && lex_src[pos + 1] == 47) {
                while (lex_src[pos] != 0 && lex_src[pos] != 10) pos = pos + 1;
                continue;
            }
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
                if (nargs >= PP_MAX_ARGS) {
                    fdputs("s12cc: too many macro arguments\n", 2);
                    exit(1);
                }
                arg_start[nargs] = pos;
            } else {
                pos = pos + 1;
            }
        }
    }

    call_nl = pp_call_newlines(ins_pos, pos);
    if (nargs > pp_dnpar[di] && !pp_dvar[di]) pp_error("too many arguments to function-like macro");

    /* The parameter slots: one per argument, and for a variadic macro
     * one more, the variable arguments from the first to the last. */
    nslot = pp_dnpar[di];
    i = 0;
    while (i < nslot) {
        rfrom[i] = 0;
        rto[i] = 0;
        if (i < nargs) {
            rfrom[i] = arg_start[i];
            rto[i] = arg_start[i] + arg_len[i];
        }
        i = i + 1;
    }
    if (pp_dvar[di]) {
        rfrom[nslot] = 0;
        rto[nslot] = 0;
        if (nargs > nslot) {
            rfrom[nslot] = arg_start[nslot];
            rto[nslot] = arg_start[nargs - 1] + arg_len[nargs - 1];
        }
        nslot = nslot + 1;
    }
    i = 0;
    while (i < nslot) {
        pre_off[i] = -1;
        pre_len[i] = 0;
        need[i] = 0;
        i = i + 1;
    }

    /* Laid down once with the arguments as written; if one used outside
     * # and ## holds a macro, it is expanded and the body laid again (the
     * expansions used pp_exp too). */
    exp_len = pp_subst(di, rfrom, rto, pre_off, pre_len, need);
    base = pp_pre_top;
    again = 0;
    i = 0;
    while (i < nslot) {
        if (need[i] && pp_has_macro(rfrom[i], rto[i])) {
            pre_off[i] = pp_preexpand(rfrom[i], rto[i]);
            pre_len[i] = pp_pre_n;
            again = 1;
        }
        i = i + 1;
    }
    if (again) exp_len = pp_subst(di, rfrom, rto, pre_off, pre_len, need);
    pp_pre_top = base;
    exp_len = pp_keep_newlines(call_nl, exp_len);
    pp_exp[exp_len] = 0;
    if (getenv("S12CC_PP_DUMP")) {   /* trace every function-like expansion */
        int q;
        fdputs("PP_EXPAND ", 2); fdputs(pp_dname[di], 2); fdputs(" ins_pos=", 2); fdputuint(2, (unsigned)ins_pos);
        fdputs(" pos=", 2); fdputuint(2, (unsigned)pos); fdputs(" exp_len=", 2); fdputuint(2, (unsigned)exp_len);
        fdputs(" nargs=", 2); fdputuint(2, (unsigned)nargs); fdputs("\n  BEFORE: [", 2);
        q = ins_pos - 40; if (q < 0) q = 0;
        while (q < pos) { fdputc(lex_src[q] == 10 ? 124 : lex_src[q], 2); q = q + 1; }
        fdputs("]\n  EXP: [", 2); fdputs(pp_exp, 2); fdputs("]\n", 2);
    }

    /* Splice pp_exp into lex_src, replacing lex_src[ins_pos..pos) */
    remove_len = pos - ins_pos;
    delta = exp_len - remove_len;
    tail_len = lex_len - pos;

    /* As in pp_expand_obj: the expansion ends at `pos`, where the call's
     * closing paren ended, and runs backwards into the consumed prefix. */
    if (pp_peek_depth == 0 && ins_pos >= delta) {
        ins_pos = pos - exp_len;
        i = 0;
        while (i < exp_len) {
            lex_src[ins_pos + i] = pp_exp[i];
            i = i + 1;
        }
        /* Entries that ended at or before `pos` belong to finished
         * expansions; the scan is about to move back over the prefix
         * they sit in, and pp_disabled would still honour them there
         * (SQLite: ROUND8 inside an assert, right after a ROUND8). */
        /* Strictly before: an entry ending exactly at `pos` is the
         * enclosing expansion whose last token is this very call
         * (`#define SELF SELF`), and it must stay disabled. */
        while (pp_ndis > pp_dis_base && pp_dis_end[pp_ndis - 1] < pos) pp_ndis = pp_ndis - 1;
        pp_dis_push(di, pos);
        pp_xend = pos;
        lex_pos = ins_pos;
        pp_sync();
        return 1;
    }
    pp_dis_shift(pos - 1, delta);
    pp_dis_push(di, pos + delta);
    pp_xend = ins_pos + exp_len;
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
    int start_pos;
    int ins_pos;
    int body_len;
    int remove_len;
    int tail_len;
    int delta;
    int i;
    char *body;

    body = pp_dbody[di];
    body_len = 0;
    while (body[body_len] != 0) body_len = body_len + 1;

    ins_pos = lex_rp - lex_src;
    remove_len = lex_slen;
    start_pos = ins_pos - remove_len;
    tail_len = lex_len - ins_pos;
    delta = body_len - remove_len;

    /* The expansion ends where the macro name ended and runs BACKWARDS into
     * the consumed prefix, which nothing reads again; the unread tail stays
     * where it is.  Shifting the tail to make room cost a copy of the whole
     * remainder of the source per macro use -- on SQLite's 9MB amalgamation
     * that was trillions of byte moves and looked like a hang (the DBT's
     * PC probe put every sample in these loops).  The shift remains only
     * for the first bytes of a file, where the prefix is too short. */
    if (pp_peek_depth == 0 && start_pos >= delta) {
        start_pos = ins_pos - body_len;
        i = 0;
        while (i < body_len) {
            lex_src[start_pos + i] = body[i];
            i = i + 1;
        }
        while (pp_ndis > pp_dis_base && pp_dis_end[pp_ndis - 1] < ins_pos) pp_ndis = pp_ndis - 1;   /* as above */
        pp_dis_push(di, ins_pos);
        pp_xend = ins_pos;
        lex_pos = start_pos;
        pp_sync();
        return;
    }
    pp_dis_shift(ins_pos - 1, delta);
    pp_dis_push(di, ins_pos + delta);
    pp_xend = start_pos + body_len;
    if (delta > 0) {
        /* Shift tail right when the expansion is longer than the name. */
        i = tail_len - 1;
        while (i >= 0) {
            lex_src[ins_pos + delta + i] = lex_src[ins_pos + i];
            i = i - 1;
        }
    } else if (delta < 0) {
        /* Shift tail left when the expansion is shorter than the name. */
        i = 0;
        while (i < tail_len) {
            lex_src[ins_pos + delta + i] = lex_src[ins_pos + i];
            i = i + 1;
        }
    }

    /* Replace the identifier with the macro body text. */
    i = 0;
    while (i < body_len) {
        lex_src[start_pos + i] = body[i];
        i = i + 1;
    }

    lex_len = lex_len + delta;
    lex_src[lex_len] = 0;
    lex_pos = start_pos;
    pp_sync();
}

/* --- Directives --- */

/* Names a program may neither #define nor #undef (6.10.8p2). */
static int pp_reserved_name(char *name) {
    return strcmp(name, "defined") == 0 || strcmp(name, "__LINE__") == 0 ||
           strcmp(name, "__FILE__") == 0 || strcmp(name, "__DATE__") == 0 ||
           strcmp(name, "__TIME__") == 0 || strcmp(name, "__STDC__") == 0 ||
           strcmp(name, "__STDC_VERSION__") == 0 || strcmp(name, "__STDC_HOSTED__") == 0;
}

/* The index of name among the first `len` bytes of a packed parameter
 * list ("a\0b\0"), or -1. */
static int pp_parm_index(char *parms, int len, char *name) {
    int k;
    int pi;
    k = 0;
    pi = 0;
    while (k < len) {
        if (strcmp(parms + k, name) == 0) return pi;
        while (parms[k] != 0) k = k + 1;
        k = k + 1;
        pi = pi + 1;
    }
    return -1;
}

/* The replacement list's constraints (6.10.3p5, 6.10.3.2p1, 6.10.3.3p1):
 * __VA_ARGS__ only in a variadic macro; in a function-like macro, #
 * before a parameter; ## at neither end. */
static void pp_check_body(char *body, int is_func, int is_var, char *parms, int plen) {
    int j;
    int k;
    int c;
    int q;
    char id[256];
    j = 0;
    while (body[j] == 32 || body[j] == 9) j = j + 1;
    if (body[j] == 35 && body[j + 1] == 35) pp_error("'##' cannot appear at either end of a macro expansion");
    k = j;
    while (body[k] != 0) k = k + 1;
    while (k > j && (body[k - 1] == 32 || body[k - 1] == 9)) k = k - 1;
    if (k - j >= 2 && body[k - 1] == 35 && body[k - 2] == 35)
        pp_error("'##' cannot appear at either end of a macro expansion");
    while (body[j] != 0) {
        c = body[j];
        if (c == 34 || c == 39) {
            q = c;
            j = j + 1;
            while (body[j] != 0 && body[j] != q) {
                if (body[j] == 92 && body[j + 1] != 0) j = j + 1;
                j = j + 1;
            }
            if (body[j] != 0) j = j + 1;
            continue;
        }
        if (c == 35 && body[j + 1] == 35) { j = j + 2; continue; }
        if (c == 35 && is_func) {
            j = j + 1;
            while (body[j] == 32 || body[j] == 9) j = j + 1;
            k = 0;
            while (pp_is_idch(body[j]) && k < 255) { id[k] = body[j]; k = k + 1; j = j + 1; }
            id[k] = 0;
            if (k == 0 || (pp_parm_index(parms, plen, id) < 0 &&
                           !(is_var && strcmp(id, "__VA_ARGS__") == 0)))
                pp_error("'#' is not followed by a macro parameter");
            continue;
        }
        if (pp_is_idstart(c)) {
            k = 0;
            while (pp_is_idch(body[j])) { if (k < 255) { id[k] = body[j]; k = k + 1; } j = j + 1; }
            id[k] = 0;
            if (!is_var && strcmp(id, "__VA_ARGS__") == 0)
                pp_error("__VA_ARGS__ can only appear in a variadic macro");
            continue;
        }
        if (c >= 48 && c <= 57) {   /* a pp-number: 1e5, 0x1F */
            while (pp_is_idch(body[j]) || body[j] == 46) j = j + 1;
            continue;
        }
        j = j + 1;
    }
}

/* What a redefinition is compared on (6.10.3p2): object- or function-
 * like, the parameters, and the replacement list with each run of white
 * space one space -- (1+2) and (1 + 2) differ.  Read ahead from the
 * name, the position left where it was. */
#define PP_SIG_SZ (PP_DEF_BODY_SZ + 1100)
static char pp_sig[PP_SIG_SZ];
static char pp_sig_body[PP_DEF_BODY_SZ];

static void pp_define_sig(char *name) {
    int save_pos;
    int save_line;
    int n;
    int j;
    int q;
    int sp;
    int c;
    save_pos = lex_pos;
    save_line = lex_line;
    n = 0;
    if (lex_src[lex_pos] == 40) {
        pp_sig[n] = 70;   /* 'F' */
        n = n + 1;
        while (lex_src[lex_pos] != 41 && lex_src[lex_pos] != 10 && lex_src[lex_pos] != 0 && n < 1000) {
            c = lex_src[lex_pos];
            if (c != 32 && c != 9 && c != 92 && c != 13) { pp_sig[n] = c; n = n + 1; }
            lex_pos = lex_pos + 1;
        }
        if (lex_src[lex_pos] == 41) lex_pos = lex_pos + 1;
    } else {
        pp_sig[n] = 79;   /* 'O' */
        n = n + 1;
    }
    pp_sig[n] = 32;
    n = n + 1;
    pp_skip_ws();
    pp_read_body_text(pp_sig_body);
    j = 0;
    q = 0;
    sp = 0;
    while (pp_sig_body[j] != 0 && n < PP_SIG_SZ - 2) {
        c = pp_sig_body[j];
        j = j + 1;
        if (q == 0 && (c == 32 || c == 9)) { sp = 1; continue; }
        if (sp) { pp_sig[n] = 32; n = n + 1; sp = 0; }
        if (q == 0 && (c == 34 || c == 39)) q = c;
        else if (q != 0 && c == 92 && pp_sig_body[j] != 0) {
            pp_sig[n] = c;
            n = n + 1;
            c = pp_sig_body[j];
            j = j + 1;
        } else if (q != 0 && c == q) q = 0;
        pp_sig[n] = c;
        n = n + 1;
    }
    pp_sig[n] = 0;
    lex_pos = save_pos;
    lex_line = save_line;
    j = pp_find(name);
    if (j >= 0 && pp_dsig[j] != 0 && strcmp(pp_dsig[j], pp_sig) != 0) {
        pp_where();
        fdputs("error: '", 2);
        fdputs(name, 2);
        fdputs("' macro redefined differently\n", 2);
        exit(1);
    }
}

static void pp_define_done(char *name) {
    int i;
    i = pp_find(name);
    if (pp_dsig[i] != 0) free(pp_dsig[i]);
    pp_dsig[i] = strdup(pp_sig);
}

static void pp_define(void) {
    char name[256];
    int k;
    int npar;
    int val;
    int c;
    int bi;
    int pi;
    int is_var;

    pp_skip_ws();
    pp_read_name(name);
    if (name[0] == 0) pp_error("macro name must be an identifier");
    if (pp_reserved_name(name)) pp_error("cannot #define a predefined macro or 'defined'");
    pp_define_sig(name);
    c = lex_src[lex_pos];
    /* An object-like macro's name is followed by white space (6.10.3p3):
     * #define PLUS+1 */
    if (c != 40 && c != 32 && c != 9 && c != 10 && c != 13 && c != 0 &&
        !(c == 47 && (lex_src[lex_pos + 1] == 42 || lex_src[lex_pos + 1] == 47)) &&
        !(c == 92 && (lex_src[lex_pos + 1] == 10 || lex_src[lex_pos + 1] == 13)))
        pp_error("whitespace required after the macro name");

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
                if (!pp_is_idstart(c)) pp_error("invalid macro parameter");
                k = pi;
                while ((c >= 97 && c <= 122) || (c >= 65 && c <= 90) ||
                       (c >= 48 && c <= 57) || c == 95) {
                    pp_def_parms[pi] = c;
                    pi = pi + 1;
                    lex_pos = lex_pos + 1;
                    c = lex_src[lex_pos];
                }
                pp_def_parms[pi] = 0;
                pi = pi + 1;
                if (pp_parm_index(pp_def_parms, k, pp_def_parms + k) >= 0)
                    pp_error("duplicate macro parameter name");
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
        if (lex_src[lex_pos] != 41) pp_error("missing ')' in macro parameter list");
        lex_pos = lex_pos + 1;
        pp_skip_ws();
        bi = pp_read_body_text(pp_def_body);
        pp_check_body(pp_def_body, 1, is_var, pp_def_parms, pi);
        pp_add_func(name, npar, pp_def_parms, pi, pp_def_body);
        if (is_var) {
            pp_dvar[pp_find(name)] = 1;
        }
        pp_define_done(name);
        pp_skip_line();
        pp_sync();
        return;
    }

    pp_skip_ws();
    c = lex_src[lex_pos];
    /* Check if there's a value: digit, minus, or 0x — but ONLY when
     * the number is the whole body.  `#define WEAPONTOP 32*FRACUNIT`
     * used to store 32 and silently drop `*FRACUNIT` (doom's weapon
     * never reached the top of the screen). */
    if ((c >= 48 && c <= 57) || c == 45) {
        int save_pos;
        int tail;
        int wide;
        int k;
        int nd;
        save_pos = lex_pos;
        val = (int)pp_read_int();
        /* The folded value is a 32-bit int, so a literal that carries a
         * U/L/LL suffix or has more digits than a signed int holds must
         * stay TEXT and be re-lexed with the full 64-bit state.
         * Decimal nd > 9 is 10+ digits: 1000000000 still fits, but
         * 2147483648 does not (GitHub issue 60).  libutf's
         * `#define FP_INIT 1469598103934665603ULL` came back as the
         * sign-extended low word and every fingerprint's high half was
         * wrong. */
        wide = 0;
        nd = 0;
        k = save_pos;
        if (lex_src[k] == 45) k = k + 1;
        if (lex_src[k] == 48 && (lex_src[k + 1] == 120 || lex_src[k + 1] == 88)) {
            k = k + 2;
            while (lex_src[k] == 48) k = k + 1;   /* leading zeros are free */
            while ((lex_src[k] >= 48 && lex_src[k] <= 57) ||
                   (lex_src[k] >= 97 && lex_src[k] <= 102) ||
                   (lex_src[k] >= 65 && lex_src[k] <= 70)) { nd = nd + 1; k = k + 1; }
            if (nd > 7) wide = 1;
        } else {
            while (lex_src[k] >= 48 && lex_src[k] <= 57) { nd = nd + 1; k = k + 1; }
            if (nd > 9) wide = 1;
        }
        while (k < lex_pos) {
            if (lex_src[k] == 117 || lex_src[k] == 85 ||
                lex_src[k] == 108 || lex_src[k] == 76) wide = 1;
            k = k + 1;
        }
        pp_skip_ws();
        tail = lex_src[lex_pos];
        if (wide || (tail != 10 && tail != 13 && tail != 0 &&
            !(tail == 47 && (lex_src[lex_pos + 1] == 47 ||
                             lex_src[lex_pos + 1] == 42)))) {
            /* More than a number: store the full text body. */
            lex_pos = save_pos;
            bi = pp_read_body_text(pp_def_body);
            pp_add(name, 0);
            pp_dbody[pp_find(name)] = strdup(pp_def_body);
            pp_define_done(name);
            pp_skip_line();
            pp_sync();
            return;
        }
    } else if (c == 10 || c == 13 || c == 0) {
        /* No value: an empty macro must expand to NOTHING in text
         * position (doom's `#define PACKEDATTR` branch expanded to
         * "1" and broke `} PACKEDATTR name_t;`), while still
         * existing for #ifdef.  Store an empty text body. */
        pp_add(name, 0);
        pp_dbody[pp_find(name)] = strdup("");
        pp_define_done(name);
        pp_skip_line();
        pp_sync();
        return;
    } else {
        /* Non-numeric value — store as text body */
        bi = pp_read_body_text(pp_def_body);
        pp_check_body(pp_def_body, 0, 0, pp_def_parms, 0);
        pp_add(name, 0);
        pp_dbody[pp_find(name)] = strdup(pp_def_body);
        pp_define_done(name);
        pp_skip_line();
        pp_sync();
        return;
    }
    pp_skip_line();
    pp_add(name, val);
    pp_define_done(name);
    pp_sync();
}

/* Command-line -DNAME[=value].  A bare name defines an empty text
 * body (expands to nothing, exists for #ifdef); a numeric value
 * defines an int; anything else a text body. */
static void pp_add_cmdline_def(char *name, char *val) {
    int i;
    int neg;
    int v;
    int c;

    if (val == 0 || val[0] == 0) {
        pp_add(name, 0);
        pp_dbody[pp_find(name)] = strdup("");
        return;
    }
    i = 0;
    neg = 0;
    if (val[0] == 45) { neg = 1; i = 1; }
    v = 0;
    c = val[i];
    while (c >= 48 && c <= 57) {
        v = v * 10 + (c - 48);
        i = i + 1;
        c = val[i];
    }
    if (c == 0 && i > neg) {
        if (neg) v = 0 - v;
        pp_add(name, v);
        return;
    }
    pp_add(name, 0);
    pp_dbody[pp_find(name)] = strdup(val);
}

static void pp_undef(void) {
    char name[256];
    int i;
    pp_skip_ws();
    pp_read_name(name);
    if (name[0] == 0) pp_error("macro name missing");
    if (pp_reserved_name(name)) pp_error("cannot #undef a predefined macro");
    i = pp_find(name);
    if (i >= 0) {
        free(pp_dname[i]);
        pp_dname[i] = 0;
        if (pp_dbody[i] != 0) { free(pp_dbody[i]); pp_dbody[i] = 0; }
        if (pp_dparm[i] != 0) { free(pp_dparm[i]); pp_dparm[i] = 0; }
        if (pp_dsig[i] != 0) { free(pp_dsig[i]); pp_dsig[i] = 0; }
        pp_dnpar[i] = -1;
    }
    pp_skip_line();
    pp_sync();
}

/* Record the primary source path for __FILE__ (call after pp_ndefs = 0). */
static void pp_set_source_file(char *path) {
    int i;
    i = 0;
    if (!path) {
        pp_curfile[0] = 0;
        pp_file_note();
        return;
    }
    while (path[i] != 0 && i < PP_FILE_LEN - 1) {
        pp_curfile[i] = path[i];
        i = i + 1;
    }
    pp_curfile[i] = 0;
    pp_file_note();
}

/* Static predefs available for #ifdef / #if defined().  Dynamic
 * __LINE__ / __FILE__ are handled in next() so their values track the
 * current position. */
static void pp_install_predefs(void) {
    pp_add("__STDC__", 1);
    /* 6.10.8.1: a hosted implementation (there is a full libc), and the
     * edition claimed is C11: _Static_assert, _Alignas, _Alignof,
     * _Generic, _Noreturn, _Thread_local are here (docs/TYPES.md S6).
     * Undefined, both were read as 0 -- SQLite took its pre-C99
     * FLEXARRAY, a one-element array, and two of its structures differed
     * in size from clang's.  The optional features left out are declared
     * below (6.10.8.3). */
    pp_add("__STDC_HOSTED__", 1);
    pp_add_cmdline_def("__STDC_VERSION__", "201112L");
    /* complex types and variable length arrays are optional in C11, and
     * not implemented: say so, as 6.10.8.3 has an implementation do */
    pp_add("__STDC_NO_COMPLEX__", 1);
    pp_add("__STDC_NO_VLA__", 1);
    pp_add("__S12CC__", 1);
    /* C11 7.17.1: a freestanding implementation may omit <stdatomic.h> and
       must then define this.  We have no atomics and no threads to need
       them, so say so -- portable code keys off this to pick a plain-flag
       path instead of failing to find the header. */
    pp_add("__STDC_NO_ATOMICS__", 1);
    pp_add("__STDC_NO_THREADS__", 1);     /* nor <threads.h> (7.26) */
    /* Arch-specific macros also installed from parse_program when the
     * target is known (S12CC_TARGET_A64, ty_ptr_size, …). */
}

/* Emit a TK_STRING token whose contents are `s` (for __FILE__). */
static void pp_emit_string_token(char *s) {
    int pool_start;
    int slen;
    int i;
    pool_start = lex_strpool_len;
    slen = 0;
    i = 0;
    while (s[i] != 0) {
        if (lex_strpool_len < LEX_POOL_SZ - 1) {
            lex_strpool[lex_strpool_len] = s[i];
            lex_strpool_len = lex_strpool_len + 1;
        }
        if (slen < LEX_STR_SZ - 1) {
            lex_str[slen] = s[i];
            slen = slen + 1;
        }
        i = i + 1;
    }
    if (lex_strpool_len < LEX_POOL_SZ) {
        lex_strpool[lex_strpool_len] = 0;
        lex_strpool_len = lex_strpool_len + 1;
    }
    lex_str[slen] = 0;
    if (lex_str_count < LEX_POOL_MAX) {
        lex_str_off[lex_str_count] = pool_start;
        lex_str_len[lex_str_count] = slen;
        lex_val = lex_str_count;
        lex_str_count = lex_str_count + 1;
    } else {
        lex_val = 0;
    }
    lex_tok = TK_STRING;
}

static void pp_splice_file(char *path) {
    int fd;
    int n;
    int tail_len;
    int i;
    char *tmp;
    char pre[600];
    int plen;
    char *tag;
    char *suf;
    int slen;
    int total;
    int last_slash;
    int room;

    fd = open(path, 0);
    if (fd < 0) {
        fdputs("s12cc: cannot open include: ", 2);
        fdputs(path, 2);
        fdputc(10, 2);
        exit(1);
    }
    tmp = malloc(LEX_SRC_SZ);
    if (!tmp) {
        fdputs("s12cc: out of memory for include\n", 2);
        exit(1);
    }

    /* Marker lines: "#s12cc_pushdir <path>\n" before the file's text and
       "\n#s12cc_popdir\n" after it, so a quoted #include inside the file
       resolves against the file's own directory (see pp_incdir), and
       __FILE__ and the line count are the file's own (pp_curfile). */
    last_slash = 0;
    while (path[last_slash] != 0) last_slash = last_slash + 1;
    last_slash = last_slash - 1;        /* the whole path */
    tag = "#s12cc_pushdir ";
    plen = 0;
    i = 0;
    while (tag[i] != 0) {
        pre[plen] = tag[i];
        plen = plen + 1;
        i = i + 1;
    }
    i = 0;
    while (i <= last_slash && plen < 590) {
        pre[plen] = path[i];
        plen = plen + 1;
        i = i + 1;
    }
    pre[plen] = 10;  /* '\n' */
    plen = plen + 1;
    suf = "\n#s12cc_popdir\n";
    slen = strlen(suf);

    room = LEX_SRC_SZ - lex_len - 1 - plen - slen;
    if (room < 0) room = 0;
    n = read(fd, tmp, room);
    close(fd);
    if (n < 0) n = 0;
    total = plen + n + slen;

    /* Splice: shift tail right by total, insert marker + content + marker */
    tail_len = lex_len - lex_pos;
    i = tail_len - 1;
    while (i >= 0) {
        lex_src[lex_pos + total + i] = lex_src[lex_pos + i];
        i = i - 1;
    }
    i = 0;
    while (i < plen) {
        lex_src[lex_pos + i] = pre[i];
        i = i + 1;
    }
    i = 0;
    while (i < n) {
        lex_src[lex_pos + plen + i] = tmp[i];
        i = i + 1;
    }
    i = 0;
    while (i < slen) {
        lex_src[lex_pos + plen + n + i] = suf[i];
        i = i + 1;
    }
    lex_len = lex_len + total;
    lex_src[lex_len] = 0;
    free(tmp);
    lex_bol = 1;    /* the marker is a line of its own, though no newline precedes it */
    pp_sync();
}

/* Fill path with the first place fname is found by #include's search
 * -- angle: each -I directory; quoted: the including file's directory,
 * then the source's, then the -I directories -- and say whether it was.
 * #include and __has_include share it. */
static int pp_try_path(char *path, char *dir, char *fname) {
    int pi;
    int fi;
    int fd;
    pi = 0;
    fi = 0;
    while (dir[fi] != 0) {
        path[pi] = dir[fi];
        pi = pi + 1;
        fi = fi + 1;
    }
    fi = 0;
    while (fname[fi] != 0) {
        path[pi] = fname[fi];
        pi = pi + 1;
        fi = fi + 1;
    }
    path[pi] = 0;
    fd = open(path, 0);
    if (fd < 0) return 0;
    close(fd);
    return 1;
}

static int pp_find_include(char *fname, int angle, char *path) {
    int idir;
    if (!angle) {
        /* Quoted: the including file's own directory first (C's rule --
         * libutf's include/utf/nfc.h does #include "utf_types.h" for a
         * sibling), then the top-level source directory, then the -I
         * directories like a real C preprocessor (vecscope's quoted
         * "tube.h" lives in -Iruntime/include). */
        if (pp_incdepth > 0 && pp_incdepth <= PP_MAX_INCDEPTH &&
            pp_try_path(path, pp_incdir + (pp_incdepth - 1) * 256, fname))
            return 1;
        if (pp_try_path(path, pp_sdir, fname)) return 1;
    }
    idir = 0;
    while (idir < pp_nidirs) {
        if (pp_try_path(path, pp_idir + idir * PP_IDIR_LEN, fname)) return 1;
        idir = idir + 1;
    }
    return 0;
}

/* #include NAME, NAME an object-like macro whose replacement is
 * "file" or <file> (6.10.2p4).  Returns 1 with fname and *angle set. */
static int pp_include_from_macro(char *fname, int *angle) {
    char name[256];
    char *b;
    int di;
    int fi;
    int close;
    pp_read_name(name);
    if (name[0] == 0) return 0;
    di = pp_find(name);
    if (di < 0 || pp_dnpar[di] >= 0 || pp_dbody[di] == 0) return 0;
    b = pp_dbody[di];
    if (b[0] != 34 && b[0] != 60) return 0;
    close = b[0] == 34 ? 34 : 62;
    *angle = b[0] == 60;
    fi = 0;
    while (b[fi + 1] != 0 && b[fi + 1] != close && fi < 255) {
        fname[fi] = b[fi + 1];
        fi = fi + 1;
    }
    fname[fi] = 0;
    return b[fi + 1] == close;
}

/* Files that said #pragma once, by the path they were included as. */
#define PP_MAX_ONCE 512
static char *pp_once[PP_MAX_ONCE];
static int pp_nonce;

static int pp_is_once(char *path) {
    int i;
    i = 0;
    while (i < pp_nonce) {
        if (strcmp(pp_once[i], path) == 0) return 1;
        i = i + 1;
    }
    return 0;
}

static void pp_include(void) {
    char path[512];
    char fname[256];
    int fi;
    int angle;

    pp_skip_ws();
    angle = 0;
    if (lex_src[lex_pos] == 34) {  /* '"' */
        lex_pos = lex_pos + 1;
        /* Extract filename */
        fi = 0;
        while (lex_src[lex_pos] != 34 && lex_src[lex_pos] != 0 && lex_src[lex_pos] != 10) {
            fname[fi] = lex_src[lex_pos];
            fi = fi + 1;
            lex_pos = lex_pos + 1;
        }
        fname[fi] = 0;
        if (lex_src[lex_pos] == 34) lex_pos = lex_pos + 1;
    } else if (lex_src[lex_pos] == 60) {  /* '<' */
        angle = 1;
        lex_pos = lex_pos + 1;
        fi = 0;
        while (lex_src[lex_pos] != 62 && lex_src[lex_pos] != 0 && lex_src[lex_pos] != 10) {
            fname[fi] = lex_src[lex_pos];
            fi = fi + 1;
            lex_pos = lex_pos + 1;
        }
        fname[fi] = 0;
        if (lex_src[lex_pos] == 62) lex_pos = lex_pos + 1;
    } else if (pp_include_from_macro(fname, &angle)) {
        /* fname and angle are set */
    } else {
        /* After replacement the operand must be "..." or <...>
         * (6.10.2p4); `#include NAME` with NAME a number, or nothing,
         * was skipped. */
        pp_error("expected \"FILENAME\" or <FILENAME>");
    }
    pp_skip_line();

    if (pp_find_include(fname, angle, path)) {
        if (!pp_is_once(path)) pp_splice_file(path);
        else pp_sync();
        return;
    }
    fdputs("s12cc: cannot open include: ", 2);
    fdputs(fname, 2);
    fdputc(10, 2);
    exit(1);
}

/* --- #if expression evaluator --- */

/* __has_include(<name>) or __has_include("name"), just read: 1 when
 * #include would find it (C23 6.10.2; GCC and clang had it first). */
static int pp_ev_has_include(void) {
    char path[512];
    char fname[256];
    int fi;
    int angle;
    int close_ch;
    pp_skip_ws();
    if (lex_src[lex_pos] != 40) return 0;          /* '(' */
    lex_pos = lex_pos + 1;
    pp_skip_ws();
    angle = 0;
    close_ch = 34;
    if (lex_src[lex_pos] == 60) { angle = 1; close_ch = 62; }
    else if (lex_src[lex_pos] != 34) return 0;
    lex_pos = lex_pos + 1;
    fi = 0;
    while (lex_src[lex_pos] != close_ch && lex_src[lex_pos] != 0 &&
           lex_src[lex_pos] != 10 && fi < 255) {
        fname[fi] = lex_src[lex_pos];
        fi = fi + 1;
        lex_pos = lex_pos + 1;
    }
    fname[fi] = 0;
    if (lex_src[lex_pos] == close_ch) lex_pos = lex_pos + 1;
    pp_skip_ws();
    if (lex_src[lex_pos] == 41) lex_pos = lex_pos + 1;   /* ')' */
    return pp_find_include(fname, angle, path);
}

/* Arithmetic is long long (GitHub issue 60).  The public result of
 * pp_ev_expr stays an int 0/1 so a `#if` condition does not depend
 * on a long-long return ABI; defined() still returns 1 as long long
 * inside the evaluator, which stage07 now wraps (cd14ef67). */

/* Forward declarations for recursive descent */
static long long pp_ev_cond(void);

static int pp_ev_depth;
static int pp_ev_park;          /* grow-down parking in unused lex_src tail */
static int pp_ev_dis[32];       /* macros being expanded in this #if */
static int pp_ev_ndis;

/* Evaluate a text-bodied object-like macro as its own expression.
 * Overlaying the body onto lex_src at (lex_pos - strlen(body)) clobbered
 * the #if line and only ran pp_ev_primary, so 1+2 * 3 was 1+(2*3) and a
 * 10-digit body with a 1-char name near the start of a file was 0
 * (GitHub issue 45). */
static long long pp_ev_from_text(char *body) {
    int blen;
    int i;
    int place;
    int saved_pos;
    int saved_park;
    long long val;

    blen = 0;
    while (body[blen] != 0) blen = blen + 1;
    if (blen < 1) return 0;
    if (pp_ev_park == 0) pp_ev_park = LEX_SRC_SZ;
    place = pp_ev_park - blen - 2;
    if (place <= lex_len) return 0;
    i = 0;
    while (i < blen) {
        lex_src[place + i] = body[i];
        i = i + 1;
    }
    lex_src[place + blen] = 10;
    lex_src[place + blen + 1] = 0;
    saved_park = pp_ev_park;
    pp_ev_park = place;
    saved_pos = lex_pos;
    lex_pos = place;
    val = pp_ev_cond();
    lex_pos = saved_pos;
    pp_ev_park = saved_park;
    return val;
}

/* A function-like macro in #if: its call replaced and the result
 * evaluated (6.10.1p4).  The name alone, with no '(', is an identifier,
 * 0.  SQLite's `#if GCC_VERSION>=4007000 || __has_extension(c_atomic)`
 * with its own `#define __has_extension(x) 0` gave 0 by luck: the call
 * was dropped with the rest of the line. */
static long long pp_ev_call(int di) {
    int rfrom[PP_MAX_ARGS + 1];
    int rto[PP_MAX_ARGS + 1];
    int pre_off[PP_MAX_ARGS + 1];
    int pre_len[PP_MAX_ARGS + 1];
    int need[PP_MAX_ARGS + 1];
    int nargs;
    int depth;
    int nslot;
    int c;
    int i;
    int k;
    char *text;
    long long val;
    pp_skip_ws();
    if (lex_src[lex_pos] != 40) return 0;
    lex_pos = lex_pos + 1;
    pp_skip_ws();
    nargs = 0;
    depth = 0;
    if (lex_src[lex_pos] == 41) {
        lex_pos = lex_pos + 1;
    } else {
        rfrom[0] = lex_pos;
        while (1) {
            c = lex_src[lex_pos];
            if (c == 10 || c == 0) pp_error("unterminated macro call in #if");
            if (c == 34 || c == 39) { lex_pos = pp_skip_literal(lex_pos, lex_len); continue; }
            if (c == 40) depth = depth + 1;
            if ((c == 41 && depth == 0) || (c == 44 && depth == 0)) {
                if (nargs >= PP_MAX_ARGS) pp_error("too many macro arguments");
                rto[nargs] = lex_pos;
                nargs = nargs + 1;
                lex_pos = lex_pos + 1;
                if (c == 41) break;
                pp_skip_ws();
                rfrom[nargs] = lex_pos;
                continue;
            }
            if (c == 41) depth = depth - 1;
            lex_pos = lex_pos + 1;
        }
    }
    nslot = pp_dnpar[di];
    if (nargs > nslot && !pp_dvar[di]) pp_error("too many arguments to function-like macro");
    if (nargs < nslot && !(nslot == 1 && nargs == 0)) pp_error("too few arguments to function-like macro");
    i = nargs;
    while (i < nslot) { rfrom[i] = 0; rto[i] = 0; i = i + 1; }
    if (pp_dvar[di]) {
        if (nargs > nslot) rto[nslot] = rto[nargs - 1];
        else { rfrom[nslot] = 0; rto[nslot] = 0; }
        nslot = nslot + 1;
    }
    i = 0;
    while (i < nslot) {
        pre_off[i] = -1;
        pre_len[i] = 0;
        need[i] = 0;
        i = i + 1;
    }
    k = pp_subst(di, rfrom, rto, pre_off, pre_len, need);
    pp_exp[k] = 0;
    if (k == 0) pp_error("macro in #if expands to nothing");
    text = strdup(pp_exp);
    k = 0;
    while (k < pp_ev_ndis) {
        if (pp_ev_dis[k] == di) pp_error("recursive macro in #if expression");
        k = k + 1;
    }
    if (pp_ev_ndis >= 32) pp_error("macros nested too deeply in #if expression");
    pp_ev_dis[pp_ev_ndis] = di;
    pp_ev_ndis = pp_ev_ndis + 1;
    val = pp_ev_from_text(text);
    pp_ev_ndis = pp_ev_ndis - 1;
    free(text);
    return val;
}

static long long pp_ev_primary(void) {
    long long val;
    int c;
    char name[256];
    pp_skip_ws();
    c = lex_src[lex_pos];
    pp_ev_u = 0;
    /* Parenthesized expression */
    if (c == 40) {  /* '(' */
        lex_pos = lex_pos + 1;
        val = pp_ev_cond();
        pp_skip_ws();
        if (lex_src[lex_pos] != 41) pp_error("missing ')' in #if expression");
        lex_pos = lex_pos + 1;
        return val;
    }
    /* Integer literal (decimal or hex) */
    if ((c >= 48 && c <= 57) || c == 45) {
        return pp_read_int();
    }
    /* Character constant: 'x' */
    if (c == 39) {  /* single quote */
        int ep;
        lex_pos = lex_pos + 1;
        if (lex_src[lex_pos] == 92) {
            /* '\n', '\x41', '\101': the escape was read as a backslash,
             * and the rest skipped with the line */
            ep = lex_pos + 1;
            val = lex_parse_esc(lex_src, &ep);
            lex_pos = ep;
        } else {
            val = lex_src[lex_pos];
            lex_pos = lex_pos + 1;
        }
        if (val > 127) val = val - 256;   /* char is signed, as in the lexer */
        if (lex_src[lex_pos] != 39) pp_error("invalid character constant in #if");
        lex_pos = lex_pos + 1;
        return val;
    }
    /* Identifier: 'defined' or macro name */
    if ((c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c == 95) {
        pp_read_name(name);
        if (strcmp(name, "__has_include") == 0) return pp_ev_has_include();
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
            if (name[0] == 0) pp_error("defined requires an identifier");
            if (has_paren) {
                pp_skip_ws();
                if (lex_src[lex_pos] != 41) pp_error("missing ')' after defined");
                lex_pos = lex_pos + 1;
            }
            return pp_is_defined(name);
        }
        /* Regular macro — expand to value, or 0 if undefined */
        val = pp_find(name);
        if (val >= 0 && pp_dnpar[val] >= 0) return pp_ev_call((int)val);
        if (val >= 0) {
            if (pp_dbody[val] != 0 && pp_dnpar[val] < 0 && pp_ev_depth < 32) {
                /* Text body: park it in the unused lex_src tail and parse
                 * as a full expression (GitHub issue 45). */
                int k;
                int di;
                di = val;
                k = 0;
                while (k < pp_ev_ndis) {
                    if (pp_ev_dis[k] == di) return pp_dval[di];
                    k = k + 1;
                }
                if (pp_ev_ndis >= 32) return pp_dval[di];
                pp_ev_dis[pp_ev_ndis] = di;
                pp_ev_ndis = pp_ev_ndis + 1;
                pp_ev_depth = pp_ev_depth + 1;
                val = pp_ev_from_text(pp_dbody[di]);
                pp_ev_depth = pp_ev_depth - 1;
                pp_ev_ndis = pp_ev_ndis - 1;
                return val;
            }
            return pp_dval[val];
        }
        return 0;
    }
    pp_error("invalid token in #if expression");
    return 0;
}

/* One binary operator under the usual arithmetic conversions of C11
 * 6.10.1p4: either operand in uintmax_t makes the arithmetic and the
 * comparison unsigned; a shift has its left operand's type; a
 * comparison is a signed 0 or 1.  -1 < 0u was true, and
 * 0xFFFFFFFFFFFFFFFF >> 63 was -1. */
static long long pp_ev_apply(int op, long long a, long long b, int lu, int ru) {
    unsigned long long ua;
    unsigned long long ub;
    int u;
    ua = (unsigned long long)a;
    ub = (unsigned long long)b;
    u = lu || ru;
    pp_ev_u = u;
    if (op == 42) return a * b;
    if (op == 43) return a + b;
    if (op == 45) return a - b;
    if (op == 38) return a & b;
    if (op == 94) return a ^ b;
    if (op == 124) return a | b;
    if (op == 47 || op == 37) {
        if (b == 0) return 0;
        if (u && op == 47) return (long long)(ua / ub);
        if (u) return (long long)(ua % ub);
        if (op == 47) return a / b;
        return a % b;
    }
    if (op == 108 || op == 114) {   /* << >> */
        pp_ev_u = lu;
        if (op == 108) return a << (int)b;
        if (lu) return (long long)(ua >> (int)b);
        return a >> (int)b;
    }
    pp_ev_u = 0;
    if (u) {
        if (op == 60) return ua < ub;
        if (op == 62) return ua > ub;
        if (op == 76) return ua <= ub;
        return ua >= ub;
    }
    if (op == 60) return a < b;
    if (op == 62) return a > b;
    if (op == 76) return a <= b;
    return a >= b;
}

static long long pp_ev_unary(void) {
    int c;
    long long v;
    pp_skip_ws();
    c = lex_src[lex_pos];
    if (c == 33) {  /* '!' */
        lex_pos = lex_pos + 1;
        v = !pp_ev_unary();
        pp_ev_u = 0;
        return v;
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

static long long pp_ev_mul(void) {
    long long val;
    int c;
    int lu;
    val = pp_ev_unary();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 42 || c == 47 || c == 37) {  /* '*', '/', '%' */
        lex_pos = lex_pos + 1;
        lu = pp_ev_u;
        val = pp_ev_apply(c, val, pp_ev_unary(), lu, pp_ev_u);
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static long long pp_ev_add(void) {
    long long val;
    int c;
    int lu;
    val = pp_ev_mul();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 43 || c == 45) {  /* '+', '-' */
        lex_pos = lex_pos + 1;
        lu = pp_ev_u;
        val = pp_ev_apply(c, val, pp_ev_mul(), lu, pp_ev_u);
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static long long pp_ev_shift(void) {
    long long val;
    int c;
    int lu;
    val = pp_ev_add();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while ((c == 60 && lex_src[lex_pos + 1] == 60) ||
           (c == 62 && lex_src[lex_pos + 1] == 62)) {  /* '<<', '>>' */
        lex_pos = lex_pos + 2;
        lu = pp_ev_u;
        val = pp_ev_apply(c == 60 ? 108 : 114, val, pp_ev_add(), lu, pp_ev_u);
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static long long pp_ev_rel(void) {
    long long val;
    int c;
    int c2;
    int op;
    int lu;
    val = pp_ev_shift();
    pp_skip_ws();
    c = lex_src[lex_pos];
    c2 = lex_src[lex_pos + 1];
    while ((c == 60 && c2 != 60) || (c == 62 && c2 != 62)) {  /* < > <= >= */
        op = c;
        lex_pos = lex_pos + 1;
        if (c2 == 61) {
            op = c == 60 ? 76 : 71;
            lex_pos = lex_pos + 1;
        }
        lu = pp_ev_u;
        val = pp_ev_apply(op, val, pp_ev_shift(), lu, pp_ev_u);
        pp_skip_ws();
        c = lex_src[lex_pos];
        c2 = lex_src[lex_pos + 1];
    }
    return val;
}

static long long pp_ev_eq(void) {
    long long val;
    int c;
    val = pp_ev_rel();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while ((c == 61 && lex_src[lex_pos + 1] == 61) ||  /* '==' */
           (c == 33 && lex_src[lex_pos + 1] == 61)) {  /* '!=' */
        lex_pos = lex_pos + 2;
        if (c == 61) val = val == pp_ev_rel();
        else val = val != pp_ev_rel();
        pp_ev_u = 0;
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static long long pp_ev_bitand(void) {
    long long val;
    int c;
    int lu;
    val = pp_ev_eq();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 38 && lex_src[lex_pos + 1] != 38) {  /* '&' but not '&&' */
        lex_pos = lex_pos + 1;
        lu = pp_ev_u;
        val = pp_ev_apply(c, val, pp_ev_eq(), lu, pp_ev_u);
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static long long pp_ev_bitxor(void) {
    long long val;
    int c;
    int lu;
    val = pp_ev_bitand();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 94) {  /* '^' */
        lex_pos = lex_pos + 1;
        lu = pp_ev_u;
        val = pp_ev_apply(c, val, pp_ev_bitand(), lu, pp_ev_u);
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static long long pp_ev_bitor(void) {
    long long val;
    int c;
    int lu;
    val = pp_ev_bitxor();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 124 && lex_src[lex_pos + 1] != 124) {  /* '|' but not '||' */
        lex_pos = lex_pos + 1;
        lu = pp_ev_u;
        val = pp_ev_apply(c, val, pp_ev_bitxor(), lu, pp_ev_u);
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static long long pp_ev_and(void) {
    long long val;
    int c;
    val = pp_ev_bitor();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 38 && lex_src[lex_pos + 1] == 38) {  /* '&&' */
        lex_pos = lex_pos + 2;
        val = pp_ev_bitor() && val;
        pp_ev_u = 0;
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

static long long pp_ev_or(void) {
    long long val;
    int c;
    val = pp_ev_and();
    pp_skip_ws();
    c = lex_src[lex_pos];
    while (c == 124 && lex_src[lex_pos + 1] == 124) {  /* '||' */
        lex_pos = lex_pos + 2;
        val = pp_ev_and() || val;
        pp_ev_u = 0;
        pp_skip_ws();
        c = lex_src[lex_pos];
    }
    return val;
}

/* c ? a : b.  There was no rule for it: `(1 ? 2 : 3) == 2` came out
 * right only because the stray tokens after the 1 were skipped.  Both
 * arms are evaluated (a division by zero is 0 here), and the result
 * has their common type. */
static long long pp_ev_cond(void) {
    long long val;
    long long a;
    long long b;
    int au;
    val = pp_ev_or();
    pp_skip_ws();
    if (lex_src[lex_pos] != 63) return val;   /* '?' */
    lex_pos = lex_pos + 1;
    a = pp_ev_cond();
    au = pp_ev_u;
    pp_skip_ws();
    if (lex_src[lex_pos] == 58) lex_pos = lex_pos + 1;   /* ':' */
    b = pp_ev_cond();
    pp_ev_u = au || pp_ev_u;
    if (val) return a;
    return b;
}

static int pp_ev_at_eol(void) {
    pp_skip_ws();
    return lex_src[lex_pos] == 10 || lex_src[lex_pos] == 13 || lex_src[lex_pos] == 0;
}

/* The whole line is one constant expression (6.10.1p1): `#if` alone,
 * `#if 1 +`, `#if 1 = 1` and `#if 1.5 > 1` were taken -- the tokens the
 * evaluator could not read were skipped with the line. */
static int pp_ev_expr(void) {
    long long val;
    pp_ev_ndis = 0;
    if (pp_ev_at_eol()) pp_error("#if with no expression");
    val = pp_ev_cond();
    if (!pp_ev_at_eol()) pp_error("invalid token in #if expression");
    pp_skip_line();
    pp_sync();
    return val != 0;
}

/* --- Directives --- */

/* "#s12cc_pushdir <path>": an included file begins.  Its directory
 * goes on pp_incdir; the includer's name and line are saved and the
 * file's own take over -- the marker's newline, still to be read, makes
 * its first line 1. */
static void pp_pushdir(void) {
    int i;
    int d;
    int last_slash;
    pp_skip_ws();
    d = pp_incdepth;
    if (d < PP_MAX_INCDEPTH) {
        memcpy(pp_incfile + d * PP_FILE_LEN, pp_curfile, PP_FILE_LEN);
        pp_incline[d] = lex_line;
        pp_incif[d] = pp_dep;
        i = 0;
        last_slash = -1;
        while (lex_src[lex_pos] != 10 && lex_src[lex_pos] != 0 && i < PP_FILE_LEN - 1) {
            pp_curfile[i] = lex_src[lex_pos];
            if (i < 255) pp_incdir[d * 256 + i] = lex_src[lex_pos];
            if (lex_src[lex_pos] == 47) last_slash = i;   /* '/' */
            i = i + 1;
            lex_pos = lex_pos + 1;
        }
        pp_curfile[i] = 0;
        pp_file_note();
        if (last_slash > 254) last_slash = -1;
        pp_incdir[d * 256 + last_slash + 1] = 0;   /* the directory: up to its last '/' */
        lex_line = 0;
    }
    pp_incdepth = d + 1;  /* counted past the cap too, so pops balance */
    pp_skip_line();
    pp_sync();
}

/* "#s12cc_popdir": the included file ended.  The includer's name and
 * line come back: two newlines are still to be read, the marker's and
 * the #include line's own, before the line after the #include. */
static void pp_popdir(void) {
    int d;
    if (pp_incdepth > 0) pp_incdepth = pp_incdepth - 1;
    d = pp_incdepth;
    /* An if-section ends in the file it began in (6.10p1, 6.10.2p3). */
    if (d < PP_MAX_INCDEPTH && pp_dep > pp_incif[d]) pp_error("unterminated conditional directive");
    if (d < PP_MAX_INCDEPTH) {
        memcpy(pp_curfile, pp_incfile + d * PP_FILE_LEN, PP_FILE_LEN);
        pp_file_note();
        lex_line = pp_incline[d] - 1;
    }
    pp_skip_line();
    pp_sync();
}

/* #line digits ["name"] (6.10.4): the next line is line digits, of file
 * name.  The operands may come from macros (6.10.4p5): an identifier is
 * replaced by its definition, an int or a body of digits or a string.
 * It was skipped.  Anything malformed is skipped as before. */
static void pp_line_directive(void) {
    char word[256];
    char *src;
    int di;
    int n;
    int i;
    int k;
    pp_skip_ws();
    n = -1;
    i = 0;
    while (lex_src[lex_pos] >= 48 && lex_src[lex_pos] <= 57) {
        if (n < 0) n = 0;
        /* 1 to 2147483647 (6.10.4p3) */
        if (n > 214748364 || (n == 214748364 && lex_src[lex_pos] > 55))
            pp_error("#line number out of range");
        n = n * 10 + (lex_src[lex_pos] - 48);
        lex_pos = lex_pos + 1;
    }
    if (n < 0) {
        pp_read_name(word);
        di = word[0] != 0 ? pp_find(word) : -1;
        if (di >= 0 && pp_dnpar[di] < 0) {
            if (pp_dbody[di] == 0) n = pp_dval[di];
            else n = atoi(pp_dbody[di]);
        }
    }
    if (n < 0) pp_error("#line directive requires a digit sequence");
    if (n == 0) pp_error("#line number out of range");
    pp_skip_ws();
    src = 0;
    k = lex_pos;
    if (lex_src[lex_pos] == 34) {
        src = lex_src + lex_pos;
    } else {
        pp_read_name(word);
        di = word[0] != 0 ? pp_find(word) : -1;
        if (di >= 0 && pp_dnpar[di] < 0 && pp_dbody[di] != 0 && pp_dbody[di][0] == 34)
            src = pp_dbody[di];
        else if (word[0] != 0 || !pp_ev_at_eol())
            pp_error("invalid filename for #line directive");
    }
    if (src != 0) {
        /* the string's contents, escapes left as written */
        i = 0;
        k = 1;
        while (src[k] != 34 && src[k] != 0 && src[k] != 10 && i < PP_FILE_LEN - 1) {
            pp_curfile[i] = src[k];
            i = i + 1;
            k = k + 1;
        }
        pp_curfile[i] = 0;
        pp_file_note();
    }
    lex_line = n - 1;       /* the directive's newline makes the next line n */
    pp_skip_line();
    pp_sync();
}

/* The conditional state at each parser lexer mark (ps_lex_mark).  A
 * peek that lexes past a directive runs it, and the rewind lexes it
 * again: SQLite's `= {` then #ifdef SQLITE_ASCII (the braced-string
 * peek) pushed its level twice, and pp_dep stayed one deep to the end
 * of the file -- unseen until an unterminated #if became an error. */
#define PP_MK_MAX 8
static int pp_mk_dep[PP_MK_MAX];
static int pp_mk_skip[PP_MK_MAX];
static int pp_mk_incdepth[PP_MK_MAX];
static int pp_mk_stk[PP_MK_MAX * PP_MAX_IF];
static int pp_mk_taken[PP_MK_MAX * PP_MAX_IF];
static int pp_mk_elsed[PP_MK_MAX * PP_MAX_IF];

static void pp_cond_save(int d) {
    int i;
    pp_mk_dep[d] = pp_dep;
    pp_mk_skip[d] = pp_skip;
    pp_mk_incdepth[d] = pp_incdepth;
    i = 0;
    while (i < pp_dep) {
        pp_mk_stk[d * PP_MAX_IF + i] = pp_stk[i];
        pp_mk_taken[d * PP_MAX_IF + i] = pp_taken[i];
        pp_mk_elsed[d * PP_MAX_IF + i] = pp_elsed[i];
        i = i + 1;
    }
}

static void pp_cond_restore(int d) {
    int i;
    pp_dep = pp_mk_dep[d];
    pp_skip = pp_mk_skip[d];
    pp_incdepth = pp_mk_incdepth[d];
    i = 0;
    while (i < pp_dep) {
        pp_stk[i] = pp_mk_stk[d * PP_MAX_IF + i];
        pp_taken[i] = pp_mk_taken[d * PP_MAX_IF + i];
        pp_elsed[i] = pp_mk_elsed[d * PP_MAX_IF + i];
        i = i + 1;
    }
}

/* The if-section's shape (6.10.1): #elif and #else inside one, #else
 * once and last, #endif closing one -- in a skipped group too. */
static void pp_cond_check(char *name) {
    int el;
    int ei;
    el = strcmp(name, "else") == 0;
    ei = strcmp(name, "elif") == 0;
    if (!el && !ei && strcmp(name, "endif") != 0) return;
    if (pp_dep == 0) {
        if (el) pp_error("#else without #if");
        if (ei) pp_error("#elif without #if");
        pp_error("#endif without #if");
    }
    if ((el || ei) && pp_elsed[pp_dep - 1]) pp_error(el ? "#else after #else" : "#elif after #else");
    if (el) pp_elsed[pp_dep - 1] = 1;
}

static void pp_directive(void) {
    char name[256];
    int i;

    pp_skip_ws();
    pp_read_name(name);
    pp_cond_check(name);

    /* When skipping, only track nesting and check elif/else transitions */
    if (pp_skip) {
        /* An included file that ends inside a skipped group: its popdir
         * marker is met while skipping. */
        if (strcmp(name, "s12cc_popdir") == 0) pp_error("unterminated conditional directive");
        if (strcmp(name, "ifdef") == 0 || strcmp(name, "ifndef") == 0 ||
            strcmp(name, "if") == 0) {
            if (pp_dep >= PP_MAX_IF) {
                fdputs("s12cc: #if nesting too deep\n", 2);
                exit(1);
            }
            pp_stk[pp_dep] = pp_skip;
            pp_elsed[pp_dep] = 0;
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
            fdputs("s12cc: #if nesting too deep\n", 2);
            exit(1);
        }
        pp_stk[pp_dep] = pp_skip;
        pp_elsed[pp_dep] = 0;
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
            fdputs("s12cc: #ifdef nesting too deep\n", 2);
            exit(1);
        }
        pp_stk[pp_dep] = pp_skip;
        pp_elsed[pp_dep] = 0;
        pp_taken[pp_dep] = 0;
        if (pp_is_defined(name)) {
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
            fdputs("s12cc: #ifdef nesting too deep\n", 2);
            exit(1);
        }
        pp_stk[pp_dep] = pp_skip;
        pp_elsed[pp_dep] = 0;
        pp_taken[pp_dep] = 0;
        if (!pp_is_defined(name)) {
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
    if (strcmp(name, "error") == 0) {
        /* Consume rest of line as the message and hard-fail. */
        pp_skip_ws();
        pp_where();
        fdputs("error: #error", 2);
        if (lex_src[lex_pos] != 0 && lex_src[lex_pos] != 10) {
            fdputs(": ", 2);
            while (lex_src[lex_pos] != 0 && lex_src[lex_pos] != 10) {
                fdputc(lex_src[lex_pos], 2);
                lex_pos = lex_pos + 1;
            }
        }
        fdputc(10, 2);
        exit(1);
    }

    /* Internal markers written by pp_splice_file (see pp_incdir). */
    if (strcmp(name, "s12cc_pushdir") == 0) {
        pp_pushdir();
        return;
    }
    if (strcmp(name, "s12cc_popdir") == 0) {
        pp_popdir();
        return;
    }
    if (strcmp(name, "line") == 0) {
        pp_line_directive();
        return;
    }

    /* #pragma once: this file is not to be included again.  It was
     * skipped with the other pragmas, and a header's function, included
     * twice, was defined twice -- unseen until that became an error. */
    if (strcmp(name, "pragma") == 0) {
        pp_skip_ws();
        pp_read_name(name);
        if (strcmp(name, "once") == 0 && pp_nonce < PP_MAX_ONCE) {
            pp_once[pp_nonce] = strdup(pp_curfile);
            pp_nonce = pp_nonce + 1;
        }
        pp_skip_line();
        pp_sync();
        return;
    }
    /* Other pragmas are implementation-defined, and skipped; a lone # is
     * the null directive.  Any other name in a live group is not a
     * directive (6.10p3): #frobnicate was skipped without a word. */
    if (strcmp(name, "pragma") != 0) {
        pp_skip_ws();
        if (name[0] != 0 || (lex_src[lex_pos] != 10 && lex_src[lex_pos] != 13 && lex_src[lex_pos] != 0))
            pp_error("invalid preprocessing directive");
    }
    pp_skip_line();
    pp_sync();
}
