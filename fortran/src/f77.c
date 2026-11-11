/* f77.c -- Fortran 77 compiler for SLOW-32.
 *
 * Single translation unit, like the rest of this project's compilers:
 * this file includes everything.  Source in, SLOW-32 assembly out.
 *
 *     f77 prog.f prog.s
 *
 * The PROGRAM unit is emitted as `main`, so the existing crt0 and
 * runtime start it and turn its return value into the process exit
 * status -- which is exactly what `STOP n` needs, with no Fortran I/O
 * runtime in the picture yet.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <math.h>      /* sqrt, fmod, ceil, floor: constant folding, exact on every host */

#include "f77_shim.h"
#include "f77_contract.h"

static void f77_error(char *msg);
static void f77_warn(char *msg);
static int f77_nerr;      /* errors reported; the parser checks it per statement */
static int f77_scan_mode; /* 1: the unit scan evaluating a kind or a length; 2: f77_len_text */
static int f77_scan_bad;  /* ... and something it could not tell */

#include "f77_card.h"
#include "f77_lexer_gen.c"
#include "hir.h"
#include "hir_alias.h"
#include "hir_ssa.h"
#include "hir_opt.h"
#include "hir_licm.h"
#include "hir_burg.h"
#include "hir_regalloc.h"
#include "f77_parse.h"
#include "hir_codegen.h"


static void f77_error(char *msg) {
    if (f77_scan_mode) { f77_scan_bad = 1; return; }
    lx_put_where(lx_stmt_line);
    fdputs(msg, 2);
    fdputc('\n', 2);
    fdputs("  in: ", 2);
    fdputs(lx_stmt, 2);
    fdputc('\n', 2);
    f77_nerr = f77_nerr + 1;
}

/* Not an error: the statement stands, and this is said of it.  Once --
 * not again where a spliced callee's text is read a second time. */
static void f77_warn(char *msg) {
    if (f77_scan_mode || f77_inline_depth > 0) return;
    lx_put_where(lx_stmt_line);
    fdputs("warning: ", 2);
    fdputs(msg, 2);
    fdputc('\n', 2);
    fdputs("  in: ", 2);
    fdputs(lx_stmt, 2);
    fdputc('\n', 2);
}

/* fort370: two subprograms (or ENTRY points) of one name -- the CALL
 * reached whichever came first, without a word (fort370 issue 13) --
 * and, on MVS, two names that are one external name there, the first
 * eight characters, which IEWL would take for one. */
static void f77_report_at(int line, char *msg) {
    lx_put_where(line);
    fdputs(msg, 2);
    fdputc('\n', 2);
    f77_nerr = f77_nerr + 1;
}
static void f77_check_unit_names(void) {
    int i;
    int j;
    char m[256];
    for (j = 0; j < f77_nunit; j++) {
        if (f77_ukind[j] != F77_UNIT_SUBR && f77_ukind[j] != F77_UNIT_FUNC) continue;
        for (i = 0; i < j; i++) {
            if (f77_ukind[i] != F77_UNIT_SUBR && f77_ukind[i] != F77_UNIT_FUNC) continue;
            {
                int a;
                int b;
                /* report at the later of the two in the source (ENTRY
                 * points are units after the ordinary ones) */
                a = f77_uline[i] <= f77_uline[j] ? i : j;
                b = a == i ? j : i;
                /* two of one name and one host; procedures of different
                 * hosts are scoped apart (their link names differ) */
                if (strcmp(f77_uname[i], f77_uname[j]) == 0 && f77_uhost[i] == f77_uhost[j]) {
                    sprintf(m, "%s is already a subprogram or ENTRY name (%s)", f77_uname[b], lx_where_text(f77_uline[a]));
                    f77_report_at(f77_uline[b], m);
                    break;
                }
#ifdef F77_TARGET_S370
                if ((strlen(f77_uname[i]) > 8 || strlen(f77_uname[j]) > 8) && strncmp(f77_uname[i], f77_uname[j], 8) == 0) {
                    sprintf(m, "%s and %s (line %d) are one external name, %.8s: MVS keeps eight characters",
                            f77_uname[b], f77_uname[a], f77_uline[a], f77_uname[b]);
                    f77_report_at(f77_uline[b], m);
                    break;
                }
#endif
            }
        }
    }
}

static Node f77_units[F77_MAX_UNIT];
static Node f77_program;

/* Phase 1: walk the source once and record where each program unit
 * begins.  A file may hold a PROGRAM plus any number of SUBROUTINEs and
 * FUNCTIONs, and the backend drives them one at a time, so their
 * boundaries have to be known before any of them is compiled. */
#define F77_MAX_ENTRY 64
static int  f77_ent_pos[F77_MAX_ENTRY];
static int  f77_ent_line[F77_MAX_ENTRY];
static int  f77_ent_parent[F77_MAX_ENTRY];
static char f77_ent_name[F77_MAX_ENTRY][F77_MAX_NAME];
static int  f77_ent_ty[F77_MAX_ENTRY];
static int  f77_ent_len[F77_MAX_ENTRY];    /* a CHARACTER result's length */
static char f77_ent_res[F77_MAX_ENTRY][F77_MAX_NAME];   /* its RESULT name, or "" */
static int  f77_ent_log[F77_MAX_ENTRY];    /* a LOGICAL result */

static int  f77_nent;

/* Names a type statement of the current unit declared, so that an ENTRY
 * statement after them takes its type: `DOUBLE PRECISION X, E2` then
 * `ENTRY E2(X)` made E2 REAL by its initial letter, and every reference to
 * it was refused ("E2 is DOUBLE PRECISION here but REAL where it is
 * defined").  An ENTRY declared after its ENTRY statement was always
 * right.  Grows; reset when the unit changes. */
static char (*f77_dn_name)[F77_MAX_NAME];
static int *f77_dn_ty;
static int f77_dn_n;
static int f77_dn_cap;
static int f77_dn_unit = -1;

/* Does the declaration being scanned carry the attribute `at` before
 * its :: (Fortran 90)?  Text-level, as the scan is. */
static int f77_has_attr(char *at) {
    int dc;
    int n;
    int i;
    dc = f77_toplevel(':', 0);
    if (dc < 0 || dc + 1 >= lx_stmt_len || lx_stmt[dc + 1] != ':') return 0;
    n = (int)strlen(at);
    for (i = 0; i + n < dc; i++)
        if (lx_stmt[i] == ',' && strncmp(lx_stmt + i + 1, at, (size_t)n) == 0 &&
            (lx_stmt[i + 1 + n] == ',' || lx_stmt[i + 1 + n] == ':')) return 1;
    return 0;
}

/* What the name being noted has besides its type: a CHARACTER length,
 * LOGICAL-ness.  An ENTRY of a FUNCTION is typed by these -- its result
 * was CHARACTER*1 and no LOGICAL whatever the unit declared. */
static int f77_dn_xlen = 1;
static int f77_dn_xlog;
static int *f77_dn_len;
static int *f77_dn_log;

static void f77_dn_note(int unit, char *nm, int ty) {
    int q;
    if (unit != f77_dn_unit) { f77_dn_unit = unit; f77_dn_n = 0; }
    for (q = 0; q < f77_nent; q++)
        if (f77_ent_parent[q] == unit && strcmp(f77_ent_name[q], nm) == 0) {
            f77_ent_ty[q] = ty; f77_ent_len[q] = f77_dn_xlen; f77_ent_log[q] = f77_dn_xlog;
        }
    if (f77_dn_n >= f77_dn_cap) {
        f77_dn_cap = f77_dn_cap ? f77_dn_cap * 2 : 128;
        f77_dn_name = realloc(f77_dn_name, (size_t)f77_dn_cap * sizeof(*f77_dn_name));
        f77_dn_ty = realloc(f77_dn_ty, (size_t)f77_dn_cap * sizeof(int));
        f77_dn_len = realloc(f77_dn_len, (size_t)f77_dn_cap * sizeof(int));
        f77_dn_log = realloc(f77_dn_log, (size_t)f77_dn_cap * sizeof(int));
        if (!f77_dn_name || !f77_dn_ty || !f77_dn_len || !f77_dn_log) { fdputs("f77: out of memory\n", 2); exit(1); }
    }
    strcpy(f77_dn_name[f77_dn_n], nm);
    f77_dn_ty[f77_dn_n] = ty;
    f77_dn_len[f77_dn_n] = f77_dn_xlen;
    f77_dn_log[f77_dn_n] = f77_dn_xlog;
    f77_dn_n = f77_dn_n + 1;
    f77_dn_xlen = 1; f77_dn_xlog = 0;
}

/* The type a type statement of `unit` gave `nm`, or -1. */
static int f77_dn_find(int unit, char *nm) {
    int q;
    if (unit != f77_dn_unit) return -1;
    for (q = f77_dn_n - 1; q >= 0; q--)
        if (strcmp(f77_dn_name[q], nm) == 0) return f77_dn_ty[q];
    return -1;
}

static int f77_dn_has(int unit, char *nm) {
    int q;
    if (unit != f77_dn_unit) return 0;
    for (q = 0; q < f77_dn_n; q++)
        if (strcmp(f77_dn_name[q], nm) == 0) return 1;
    return 0;
}

/* Is nm unit's own name -- a dummy of it, or declared by a type statement
 * the scan has read -- and so not its host's? */
static int f77_scan_local(int unit, char *nm) {
    int q;
    for (q = 0; q < f77_nfamd; q++)
        if (f77_famd_unit[q] == unit && strcmp(f77_famd_name[q], nm) == 0) return 1;
    return f77_dn_has(unit, nm);
}

/* The scan's table of INTEGER named constants (f77_scan_const), from the
 * statement being scanned: PARAMETER (N = 4, ...) in any unit, and
 * INTEGER, PARAMETER :: N = 4 outside a FUNCTION -- whose type statements
 * the scan reads anyway.  A name is INTEGER by its type statement, where
 * the scan read one, else by its initial; what this leaves out, the scan
 * cannot tell and the compilation does. */
static void f77_scan_params(int u) {
    int typed;
    typed = 0;
    if (f77_starts("PARAMETER(")) f77_scan_from(10);
    else if (f77_ukind[u] != F77_UNIT_FUNC && f77_starts("INTEGER") && f77_has_attr("PARAMETER")) {
        f77_scan_from(f77_toplevel(':', 0) + 2);
        typed = 1;
    } else return;
    f77_scan_begin(u);
    while (lx_t == T_NAME) {
        char nm[F77_MAX_NAME];
        int ty;
        strcpy(nm, lex_name);
        f77_tok();
        if (lx_t != T_ASSIGN) break;
        f77_tok();
        f77_scan_mode = 1;
        f77_scan_bad = 0;
        f77_cexpr();
        f77_scan_mode = 0;
        ty = TY_INT;
        if (!typed) {
            ty = f77_dn_find(u, nm);
            if (ty < 0) ty = f77_dn_has(u, nm) ? -1 : f77_implicit_ty(nm);
        }
        f77_scan_const_add(u, nm, f77_cx_i, !f77_scan_bad && f77_cx_ty == TY_INT && ty == TY_INT);
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
}

/* Record the dummy names of the header the scanner is on (past `skip`)
 * against unit `unit`, for the ENTRY phantoms. */
static void f77_collect_dummies(int skip, int unit) {
    f77_scan_from(skip);
    if (lx_t == T_NAME) f77_tok();
    f77_hdr_skip_len();
    if (lx_t != T_LP) return;
    f77_tok();
    while (lx_t == T_NAME || lx_t == T_STAR) {
        if (lx_t == T_NAME && f77_nfamd < F77_MAX_FAMD) {
            strcpy(f77_famd_name[f77_nfamd], lex_name);
            f77_famd_unit[f77_nfamd] = unit;
            f77_famd_as[f77_nfamd] = 0;
            f77_famd_opt[f77_nfamd] = 0;
            f77_famd_ptr[f77_nfamd] = 0;
            f77_famd_char[f77_nfamd] = 0;
            f77_famd_tytext[f77_nfamd] = NULL;
            f77_famd_rank[f77_nfamd] = 0;
            f77_nfamd = f77_nfamd + 1;
        }
        f77_tok();
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
}

/* Might the declaration being scanned declare an automatic array -- an
 * entity that is not a dummy of unit u, with a name inside its
 * dimensions?  Such a unit is not inlined: the splice has no place to
 * allocate it.  A PARAMETER in a bound answers yes too, which costs only
 * the inline. */
static int f77_scan_may_auto(int u) {
    int i;
    int skip;
    if (f77_starts("DIMENSION")) skip = 9;
    else if (f77_starts("DOUBLEPRECISION")) skip = 15;
    else if (f77_starts("DOUBLECOMPLEX")) skip = 13;
    else if (f77_starts("INTEGER") || f77_starts("LOGICAL") || f77_starts("COMPLEX")) skip = 7;
    else if (f77_starts("REAL")) skip = 4;
    else return 0;                   /* CHARACTER's lengths are no arrays' */
    i = skip;
    {
        int dc;
        dc = f77_toplevel(':', 0);
        if (dc >= 0 && dc + 1 < lx_stmt_len && lx_stmt[dc + 1] == ':') i = dc + 2;
        else if (i < lx_stmt_len && lx_stmt[i] == '(') { int d = 0; for (; i < lx_stmt_len; i++) { if (lx_stmt[i] == '(') d++; else if (lx_stmt[i] == ')' && --d == 0) { i++; break; } } }
        else if (i < lx_stmt_len && lx_stmt[i] == '*') { i++; while (i < lx_stmt_len && lx_isdigit(lx_stmt[i])) i++; }
    }
    while (i < lx_stmt_len) {
        char nm[F77_MAX_NAME];
        int n;
        int q;
        int dummy;
        n = 0;
        while (i < lx_stmt_len && (lx_isalpha(lx_stmt[i]) || lx_isdigit(lx_stmt[i]) || lx_stmt[i] == '_')) {
            if (n < F77_MAX_NAME - 1) nm[n++] = lx_stmt[i];
            i++;
        }
        nm[n] = 0;
        if (n == 0) { i++; continue; }
        if (i < lx_stmt_len && lx_stmt[i] == '(') {
            int d;
            int alpha;
            d = 0;
            alpha = 0;
            for (; i < lx_stmt_len; i++) {
                if (lx_stmt[i] == '(') d++;
                else if (lx_stmt[i] == ')') { if (--d == 0) { i++; break; } }
                else if (lx_isalpha(lx_stmt[i])) alpha = 1;
            }
            dummy = 0;
            for (q = 0; q < f77_nfamd; q++)
                if (f77_famd_unit[q] == u && strcmp(f77_famd_name[q], nm) == 0) dummy = 1;
            if (alpha && !dummy) return 1;
        }
        /* to the next entity: past a *len, an initializer, the comma */
        while (i < lx_stmt_len && lx_stmt[i] != ',') {
            if (lx_stmt[i] == '(') { int d = 0; for (; i < lx_stmt_len; i++) { if (lx_stmt[i] == '(') d++; else if (lx_stmt[i] == ')' && --d == 0) break; } }
            i++;
        }
        i++;
    }
    return 0;
}

/* The rank of the array spec whose '(' is at text offset i when it is an
 * assumed shape -- every item [lo]: -- else 0. */
static int f77_text_assumed_rank(int i) {
    int d; int colon; int rank;
    if (i >= lx_stmt_len || lx_stmt[i] != '(') return 0;
    d = 0; colon = 0; rank = 0;
    for (; i < lx_stmt_len; i++) {
        int c;
        c = lx_stmt[i];
        if (c == '(') { d++; if (d == 1) continue; }
        if (c == ')') { if (--d == 0) return colon ? rank + 1 : 0; }
        if (d != 1) continue;
        if (c == ':') { colon = 1; if (lx_stmt[i + 1] != ',' && lx_stmt[i + 1] != ')') return 0; }
        else if (c == ',') { if (!colon) return 0; colon = 0; rank++; }
    }
    return 0;
}

/* What a declaration of unit u says about shapes, for calls compiled
 * before the unit (text-level, as the scan is):
 * - its assumed-shape dummies -- A(:), or a DIMENSION(:,:) attribute on
 *   entities with no shape of their own -- take descriptors (ALLOCATABLE's
 *   (:) is no dummy's);
 * - a FUNCTION's result made an array -- F(3), a DIMENSION attribute,
 *   DIMENSION F(3) -- comes back through one (f77_afn_call). */
/* The rank of the array spec whose '(' is at text offset i: its items at
 * the top level. */
static int f77_text_spec_rank(int i) {
    int d; int rank;
    if (i >= lx_stmt_len || lx_stmt[i] != '(') return 0;
    d = 0; rank = 1;
    for (; i < lx_stmt_len; i++) {
        if (lx_stmt[i] == '(') d++;
        else if (lx_stmt[i] == ')') { if (--d == 0) return rank; }
        else if (lx_stmt[i] == ',' && d == 1) rank++;
    }
    return 0;
}

static int f77_skip_parens(int i) {
    int d = 0;
    for (; i < lx_stmt_len; i++) { if (lx_stmt[i] == '(') d++; else if (lx_stmt[i] == ')' && --d == 0) return i + 1; }
    return i;
}
static void f77_scan_shapes(int u) {
    int i; int skip; int dattr; int dc; int alloc; int opt; int isch; int ptr; int tend;
    if (f77_starts("OPTIONAL") || f77_starts("POINTER")) {
        /* OPTIONAL [::] names: the dummies a caller may leave out;
         * POINTER [::] names: those whose slots it passes */
        int isp;
        isp = lx_stmt[0] == 'P';
        i = isp ? 7 : 8;
        if (i + 1 < lx_stmt_len && lx_stmt[i] == ':' && lx_stmt[i + 1] == ':') i = i + 2;
        while (i < lx_stmt_len) {
            char nm[F77_MAX_NAME];
            int n; int q;
            n = 0;
            while (i < lx_stmt_len && (lx_isalpha(lx_stmt[i]) || lx_isdigit(lx_stmt[i]) || lx_stmt[i] == '_')) {
                if (n < F77_MAX_NAME - 1) nm[n++] = lx_stmt[i];
                i++;
            }
            nm[n] = 0;
            for (q = 0; q < f77_nfamd; q++)
                if (f77_famd_unit[q] == u && strcmp(f77_famd_name[q], nm) == 0) {
                    if (isp) f77_famd_ptr[q] = 1; else f77_famd_opt[q] = 1;
                    f77_unosplice[u] = 1;
                    if (isp && i < lx_stmt_len && lx_stmt[i] == '(') f77_famd_as[q] = f77_text_assumed_rank(i);
                }
            if (i < lx_stmt_len && lx_stmt[i] == '(') i = f77_skip_parens(i);
            i++;
        }
        return;
    }
    if (f77_starts("DIMENSION")) skip = 9;
    else if (f77_starts("DOUBLEPRECISION")) skip = 15;
    else if (f77_starts("DOUBLECOMPLEX")) skip = 13;
    else if (f77_starts("CHARACTER")) skip = 9;
    else if (f77_starts("INTEGER") || f77_starts("LOGICAL") || f77_starts("COMPLEX")) skip = 7;
    else if (f77_starts("REAL")) skip = 4;
    else if (f77_starts("TYPE(")) skip = f77_match_paren(4);     /* TYPE(name) */
    else return;
    alloc = f77_has_attr("ALLOCATABLE");
    opt = f77_has_attr("OPTIONAL");
    ptr = f77_has_attr("POINTER");
    isch = f77_starts("CHARACTER") != 0;
    dattr = -1;
    i = skip;
    dc = f77_toplevel(':', 0);
    if (dc >= 0 && dc + 1 < lx_stmt_len && lx_stmt[dc + 1] == ':') {
        int j;
        for (j = 0; j + 10 < dc; j++)
            if (lx_stmt[j] == ',' && strncmp(lx_stmt + j + 1, "DIMENSION(", 10) == 0) dattr = j + 10;
        i = dc + 2;
        /* the type: up to its first attribute, or the :: */
        tend = skip;
        if (tend < dc && lx_stmt[tend] == '(') tend = f77_skip_parens(tend);
        else if (tend < dc && lx_stmt[tend] == '*') {
            tend++;
            if (tend < dc && lx_stmt[tend] == '(') tend = f77_skip_parens(tend);
            while (tend < dc && lx_isdigit(lx_stmt[tend])) tend++;
        }
    } else if (i < lx_stmt_len && lx_stmt[i] == '(') {
        i = f77_skip_parens(i);                         /* REAL(8), CHARACTER(LEN=4) */
        tend = i;
    } else if (i < lx_stmt_len && lx_stmt[i] == '*') {
        i++;                                            /* REAL*8, CHARACTER*(*), CHARACTER*4 */
        if (i < lx_stmt_len && lx_stmt[i] == '(') i = f77_skip_parens(i);
        while (i < lx_stmt_len && lx_isdigit(lx_stmt[i])) i++;
        tend = i;
        if (i < lx_stmt_len && lx_stmt[i] == ',') i++;
    } else tend = i;
    if (skip == 9 && lx_stmt[0] == 'D') tend = 0;           /* DIMENSION: no type */
    while (i < lx_stmt_len) {
        char nm[F77_MAX_NAME];
        int n; int q; int so; int ra; int rs;
        n = 0;
        while (i < lx_stmt_len && (lx_isalpha(lx_stmt[i]) || lx_isdigit(lx_stmt[i]) || lx_stmt[i] == '_')) {
            if (n < F77_MAX_NAME - 1) nm[n++] = lx_stmt[i];
            i++;
        }
        nm[n] = 0;
        if (n == 0) { i++; continue; }
        if (i < lx_stmt_len && lx_stmt[i] == '*') {      /* CHARACTER C*8(3) */
            i++;
            if (i < lx_stmt_len && lx_stmt[i] == '(') i = f77_skip_parens(i);
            while (i < lx_stmt_len && lx_isdigit(lx_stmt[i])) i++;
        }
        so = (i < lx_stmt_len && lx_stmt[i] == '(') ? i : dattr;
        ra = so >= 0 && !alloc ? f77_text_assumed_rank(so) : 0;
        rs = so >= 0 ? f77_text_spec_rank(so) : 0;
        for (q = 0; q < f77_nfamd; q++) {
            if (f77_famd_unit[q] != u || strcmp(f77_famd_name[q], nm) != 0) continue;
            if (ra > 0) { f77_famd_as[q] = ra; f77_unosplice[u] = 1; }
            if (opt) { f77_famd_opt[q] = 1; f77_unosplice[u] = 1; }
            if (ptr) { f77_famd_ptr[q] = 1; f77_unosplice[u] = 1; }
            if (isch) f77_famd_char[q] = 1;
            if (rs > 0) f77_famd_rank[q] = rs;
            if (tend > 0 && !f77_famd_tytext[q]) {
                f77_famd_tytext[q] = (char *)malloc((size_t)tend + 1);
                memcpy(f77_famd_tytext[q], lx_stmt, (size_t)tend);
                f77_famd_tytext[q][tend] = 0;
            }
        }
        if (rs > 0 && f77_ukind[u] == F77_UNIT_FUNC && strcmp(nm, f77_unit_rname(u)) == 0) {
            f77_urrank[u] = rs;
            f77_unosplice[u] = 1;
        }
        while (i < lx_stmt_len && lx_stmt[i] != ',') {
            if (lx_stmt[i] == '(') { i = f77_skip_parens(i); continue; }
            i++;
        }
        i++;
    }
}

/* Does this declaration give a variable static storage -- `, SAVE ::`,
 * or `:: V = value` (Fortran 90: an initial value is SAVE), or
 * `:: P => NULL()`?  Its unit is then never spliced, as with a DATA or
 * SAVE statement: spliced, V was the caller's temporary, never set
 * (gfortran's arithmetic_if.f90 counted down from a SAVEd 2 and got -1
 * every time).  A scalar PARAMETER's `=` is no storage; a PARAMETER
 * array's is (pr19926.f90: MAP(42) of a spliced unit read the caller's
 * frame). */
static int f77_scan_decl_static(void) {
    int p;
    p = 0;
    for (;;) {
        p = f77_toplevel(':', p);
        if (p < 0) return 0;
        if (lx_stmt[p + 1] == ':') break;
        p = p + 1;
    }
    lx_stmt[p] = 0;
    if (strstr(lx_stmt, ",PARAMETER")) {
        /* a PARAMETER array is static too (its image); a scalar is not */
        int i; int depth; int eq; int dim;
        dim = strstr(lx_stmt, ",DIMENSION") != NULL;
        lx_stmt[p] = ':';
        if (dim) return 1;
        depth = 0; eq = 0;
        for (i = p + 2; i < lx_stmt_len; i++) {
            int c;
            c = lx_stmt[i];
            if (c == '\'') { i++; while (i < lx_stmt_len && lx_stmt[i] != '\'') i++; continue; }
            if (c == '(') { if (depth == 0 && !eq) return 1; depth++; }
            else if (c == ')') depth--;
            else if (depth == 0 && c == '=') eq = 1;
            else if (depth == 0 && c == ',') eq = 0;
        }
        return 0;
    }
    if (strstr(lx_stmt, ",SAVE")) { lx_stmt[p] = ':'; return 1; }
    lx_stmt[p] = ':';
    return f77_toplevel('=', p + 2) >= 0 || strstr(lx_stmt + p, "=>") != NULL;
}

/* Where, in a token that is a name, the name begins that a statement
 * keyword runs into, or 0: with the blanks gone DO X = 1, 9 is DOX=1,9
 * and PRINT FMT, K is PRINTFMT,K, each beginning with one name.  It is
 * asked of every name and not of statements, so a variable DOT offers a
 * T as well; a candidate too many costs its host a register, no more. */
static int f77_kw_operand(char *nm) {
    static char *kw[] = { "DO", "GOTO", "PRINT", "READ", "REWIND", "BACKSPACE", "ENDFILE", "RETURN", "ASSIGN", 0 };
    int q; int k;
    for (q = 0; kw[q]; q++) {
        k = (int)strlen(kw[q]);
        if (strncmp(nm, kw[q], (size_t)k) != 0) continue;
        while (lx_isdigit(nm[k])) k = k + 1;                    /* DO 10 X, ASSIGN 10 TO X */
        if (kw[q][0] == 'A') {
            if (strncmp(nm + k, "TO", 2) != 0) continue;
            k = k + 2;
        }
        if (lx_isalpha(nm[k])) return k;
    }
    return 0;
}

static void f77_hc_add(char *nm) {
    if (f77_hc_has(nm) || f77_hc_n >= F77_MAX_HC) return;
    strcpy(f77_hc_name[f77_hc_n], nm);
    f77_hc_n = f77_hc_n + 1;
}

/* The names (tokens) in the text of unit u's internal procedures, into
 * f77_hc_name; none when u is no host, or a module (whose variables are
 * static, f77_me_bind). */
static void f77_host_cands(int u) {
    int v; int k;
    f77_hc_n = 0;
    f77_hc_host = -1;
    if (f77_ukind[u] == F77_UNIT_MODULE) return;
    for (v = 0; v < f77_nunit; v++) {
        if (f77_uhost[v] != u || f77_uentry_parent[v] >= 0) continue;
        f77_hc_host = u;
        lx_pos = f77_upos[v];
        lx_line = f77_uline[v];
        while (f77_next_stmt()) {
            f77_scan_from(0);
            while (lx_t != T_EOF) {
                if (lx_t == T_NAME) {
                    f77_hc_add(lex_name);
                    k = f77_kw_operand(lex_name);
                    if (k) f77_hc_add(lex_name + k);
                }
                f77_tok();
            }
            if (f77_is_unit_end()) break;
        }
    }
}

/* An END that says what it ends says it as the header did: the unit's
 * keyword, and after it the unit's name or none.  END SUBROUTINE B closing
 * SUBROUTINE A is a slip the reader of the program does not see, and it
 * was taken without a word.  `prog` is the main program's name, which no
 * table keeps. */
static void f77_end_check(int u, char *prog) {
    static char *kw[] = { "PROGRAM", "SUBROUTINE", "FUNCTION", "BLOCKDATA", "MODULE" };
    char nm[F77_MAX_NAME];
    char msg[96 + F77_MAX_NAME];
    int n;
    if (lx_stmt_len == 3) return;
    n = (int)strlen(kw[f77_ukind[u]]);
    if (strncmp(lx_stmt + 3, kw[f77_ukind[u]], (size_t)n) != 0) {
        sprintf(msg, "this END names another kind of unit: END %s ends this one", kw[f77_ukind[u]]);
        f77_error(msg);
        return;
    }
    if (lx_stmt_len == 3 + n) return;
    if (f77_ukind[u] == F77_UNIT_PROGRAM) strcpy(nm, prog);
    else if (f77_ukind[u] == F77_UNIT_BDATA) {
        strcpy(nm, f77_uname[u] + 6);                  /* f77bd_<name>_<n> */
        *strrchr(nm, '_') = 0;
    } else strcpy(nm, f77_uname[u]);
    if ((int)strlen(nm) == lx_stmt_len - 3 - n && strncmp(lx_stmt + 3 + n, nm, strlen(nm)) == 0) return;
    if (nm[0]) sprintf(msg, "the name on this END is not its unit's (%s)", nm);
    else strcpy(msg, "the name on this END is not its unit's, which has none");
    f77_error(msg);
}

static void f77_scan_units(void) {
    char prog[F77_MAX_NAME];   /* the main program's name, for its END */
    int pos;
    int line;
    int rty;
    int started;
    int ended;        /* the unit begun last has had its END */
    int mains;        /* main programs begun */
    int hstack[4];    /* Fortran 90: the units whose CONTAINS we are past */
    int hn;
    int in_iface;     /* inside INTERFACE ... END INTERFACE */
    int in_type;      /* inside TYPE name ... END TYPE */

    prog[0] = 0;
    f77_nunit = 0;
    f77_nent = 0;
    f77_nfamd = 0;
    { int z; z = 0; while (z < F77_MAX_UNIT) { f77_uentry_parent[z] = -1; z = z + 1; } }
    f77_nformat = 0;
    f77_nasg = 0;
    f77_nesc = 0;
    { int z; z = 0; while (z < F77_MAX_UNIT) { f77_ustmts[z] = 0; f77_unosplice[z] = 0; z = z + 1; } }
    started = 0;
    ended = 0;
    mains = 0;
    hn = 0;
    in_iface = 0;
    in_type = 0;
    { int z; for (z = 0; z < F77_MAX_UNIT; z++) f77_uhost[z] = -1; }
    pos = lx_pos;
    line = lx_line;

    while (f77_next_stmt()) {
        /* Fortran 90's structure: an INTERFACE block's headers are no
         * units; CONTAINS ends its unit's body and opens a host for the
         * procedures after it; an END after the last of them is the
         * host's. */
        if (in_iface) {
            if (lx_stmt_len >= 12 && strncmp(lx_stmt, "ENDINTERFACE", 12) == 0) in_iface = 0;
            if (started && f77_nunit > 0) f77_ustmts[f77_nunit - 1]++;
            pos = lx_pos;
            line = lx_line;
            continue;
        }
        /* a derived type's definition: its components are no unit's
         * entities */
        if (in_type) {
            if (f77_starts("ENDTYPE")) in_type = 0;
            if (started && f77_nunit > 0) f77_ustmts[f77_nunit - 1]++;
            pos = lx_pos;
            line = lx_line;
            continue;
        }
        if (started && !ended && f77_is_typedef()) {
            in_type = 1;
            if (f77_nunit > 0) { f77_ustmts[f77_nunit - 1]++; f77_unosplice[f77_nunit - 1] = 1; }
            pos = lx_pos;
            line = lx_line;
            continue;
        }
        if (started && !ended && f77_starts("INTERFACE") && f77_toplevel('=', 0) < 0) {
            in_iface = 1;
            if (f77_nunit > 0) f77_unosplice[f77_nunit - 1] = 1;     /* its bodies are its own scope's */
        }
        if (f77_is_contains()) {
            if (!started || ended) { f77_error("CONTAINS outside a program unit"); return; }
            if (hn >= 3) { f77_error("procedures nested too deep"); return; }
            if (f77_ukind[f77_nunit - 1] == F77_UNIT_BDATA) { f77_error("CONTAINS in BLOCK DATA"); return; }
            hstack[hn++] = f77_nunit - 1;
            f77_unosplice[f77_nunit - 1] = 1;
            ended = 1;
            pos = lx_pos;
            line = lx_line;
            continue;
        }
        if (hn > 0 && ended && f77_is_unit_end()) {
            hn = hn - 1;                               /* the host's END */
            f77_end_check(hstack[hn], prog);
            pos = lx_pos;
            line = lx_line;
            continue;
        }
        if (hn > 0 && ended && !f77_starts("SUBROUTINE") && f77_unit_header_ty() < 0) {
            f77_error("only a SUBROUTINE or FUNCTION may follow CONTAINS");
            return;
        }
        if (f77_starts("MODULE") && !f77_starts("MODULEPROCEDURE") && f77_toplevel('=', 0) < 0) {
            if (started && !ended) { f77_error("MODULE inside a program unit: the END before it is missing"); return; }
            if (f77_nunit >= F77_MAX_UNIT) { f77_error("too many program units"); return; }
            f77_upos[f77_nunit] = pos;
            f77_uline[f77_nunit] = line;
            f77_ukind[f77_nunit] = F77_UNIT_MODULE;
            f77_urty[f77_nunit] = TY_INT;
            f77_scan_from(6);
            if (lx_t != T_NAME) { f77_error("MODULE needs a name"); return; }
            strcpy(f77_uname[f77_nunit], lex_name);
            f77_unosplice[f77_nunit] = 1;
            f77_nunit = f77_nunit + 1;
            started = 1;
            ended = 0;
            pos = lx_pos;
            line = lx_line;
            continue;
        }
        /* A PROGRAM statement inside a subprogram means the END before
         * it is missing; taken as more of that unit, the main program
         * would vanish without a word.  (In a main program a second one
         * is documentation: 40 of the FCVS files as distributed open
         * with their PROGRAM card twice.)  Found by fort370 (3b7fff3),
         * which carries this scan for S/370. */
        if (started && !ended && f77_ukind[f77_nunit - 1] != F77_UNIT_PROGRAM
            && f77_starts("PROGRAM")
            && memchr(lx_stmt, '=', (size_t)lx_stmt_len) == NULL) {
            f77_error("PROGRAM statement inside a subprogram: the END before it is missing");
            return;
        }
        /* After an END, a statement that is no subprogram's header
         * begins the main program, with a PROGRAM statement or without
         * one: the main program may follow its subprograms. */
        if (started && ended && hn == 0 && !f77_starts("SUBROUTINE") && !f77_starts("BLOCKDATA")
            && f77_unit_header_ty() < 0) {
            if (mains > 0) { f77_error("more than one main program"); return; }
            if (f77_nunit >= F77_MAX_UNIT) { f77_error("too many program units"); return; }
            f77_upos[f77_nunit] = pos;
            f77_uline[f77_nunit] = line;
            f77_ukind[f77_nunit] = F77_UNIT_PROGRAM;
            f77_urty[f77_nunit] = TY_INT;
            strcpy(f77_uname[f77_nunit], "main");
            f77_nunit = f77_nunit + 1;
            mains = mains + 1;
            ended = 0;
        }
        /* Collect FORMAT statements: a WRITE may name a label defined
         * later in the unit, so they must all be known up front. */
        if (f77_starts("FORMAT") && lx_stmt_label >= 0 &&
            f77_nformat < F77_MAX_FORMAT) {
            f77_flabel[f77_nformat] = lx_stmt_label;
            f77_funit[f77_nformat] = started ? f77_nunit - 1 : 0;
            f77_fstr[f77_nformat] =
                f77_intern_str(lx_stmt + 6, lx_stmt_len - 6);
            f77_nformat = f77_nformat + 1;
        }
        f77_esc_scan(started ? f77_nunit - 1 : 0);
        if (f77_starts("ASSIGN") && f77_nasg < F77_MAX_ASG) {
            f77_scan_from(6);
            if (lx_t == T_ICON) {
                f77_asg_lab[f77_nasg] = lex_ival;
                f77_asg_unit[f77_nasg] = started ? f77_nunit - 1 : 0;
                f77_nasg = f77_nasg + 1;
            }
        }
        if (started && f77_nunit > 0) {
            f77_ustmts[f77_nunit - 1]++;
            if (f77_starts("COMMON") || f77_starts("IMPLICIT") ||
                f77_starts("DATA") || f77_starts("SAVE") ||
                f77_starts("EQUIVALENCE") || f77_starts("PARAMETER") ||
                f77_starts("ENTRY") || f77_starts("EXTERNAL") ||
                f77_starts("CHARACTER") || f77_starts("NAMELIST"))
                f77_unosplice[f77_nunit - 1] = 1;
            /* A type statement with initial values (INTEGER L/1/) is
             * DATA: the variable is set once and carries from call to
             * call.  Spliced into a caller it became the caller's
             * temporary, never set (fort370's issue 32, ported).  A
             * false match -- a slash elsewhere at top level -- only
             * costs a missed inline. */
            /* An ALLOCATABLE array is freed at RETURN; spliced into a
             * caller that RETURN is a branch (Fortran 90). */
            if (strstr(lx_stmt, "ALLOCATABLE")) f77_unosplice[f77_nunit - 1] = 1;
            if (f77_starts("USE")) f77_unosplice[f77_nunit - 1] = 1;
            f77_scan_params(f77_nunit - 1);
            if (f77_scan_may_auto(f77_nunit - 1)) f77_unosplice[f77_nunit - 1] = 1;
            f77_scan_shapes(f77_nunit - 1);
            if ((f77_starts("INTEGER") || f77_starts("REAL") ||
                 f77_starts("LOGICAL") || f77_starts("DOUBLE") ||
                 f77_starts("COMPLEX")) && f77_toplevel('/', 0) >= 0)
                f77_unosplice[f77_nunit - 1] = 1;
            if (f77_scan_decl_static()) f77_unosplice[f77_nunit - 1] = 1;
        }
        if (started && f77_nunit > 0 && f77_starts("ENTRY") &&
            f77_ukind[f77_nunit - 1] != F77_UNIT_PROGRAM && f77_nent < F77_MAX_ENTRY) {
            f77_scan_from(5);
            if (lx_t == T_NAME) {
                strcpy(f77_ent_name[f77_nent], lex_name);
                f77_ent_pos[f77_nent] = pos;
                f77_ent_line[f77_nent] = line;
                f77_ent_parent[f77_nent] = f77_nunit - 1;
                char rn[F77_MAX_NAME];
                /* ENTRY E(...) RESULT(R): the result is R's, typed as R is */
                strcpy(rn, lex_name);
                {
                    char *r; int k;
                    r = strstr(lx_stmt, ")RESULT(");
                    k = r ? (int)strlen(r + 8) - 1 : 0;
                    f77_ent_res[f77_nent][0] = 0;
                    if (r && k > 0 && k < F77_MAX_NAME && r[8 + k] == ')') { memcpy(rn, r + 8, (size_t)k); rn[k] = 0; strcpy(f77_ent_res[f77_nent], rn); }
                }
                f77_ent_ty[f77_nent] = f77_dn_find(f77_nunit - 1, rn);
                f77_ent_len[f77_nent] = 1; f77_ent_log[f77_nent] = 0;
                if (f77_ent_ty[f77_nent] >= 0) {
                    int dq;
                    for (dq = f77_dn_n - 1; dq >= 0; dq--)
                        if (strcmp(f77_dn_name[dq], rn) == 0) {
                            f77_ent_len[f77_nent] = f77_dn_len[dq]; f77_ent_log[f77_nent] = f77_dn_log[dq];
                            break;
                        }
                }
                if (f77_ent_ty[f77_nent] < 0 && strcmp(rn, f77_unit_rname(f77_nunit - 1)) == 0) {
                    /* the unit's own result variable */
                    f77_ent_ty[f77_nent] = f77_urty[f77_nunit - 1];
                    f77_ent_len[f77_nent] = f77_urlen[f77_nunit - 1];
                    f77_ent_log[f77_nent] = f77_urlog[f77_nunit - 1];
                }
                if (f77_ent_ty[f77_nent] < 0) f77_ent_ty[f77_nent] = f77_implicit_ty(rn);
                f77_collect_dummies(5, -2 - f77_nent);   /* unit index assigned after the scan */
                f77_nent = f77_nent + 1;
            }
            f77_unosplice[f77_nunit - 1] = 1;
            pos = lx_pos;
            line = lx_line;
            continue;
        }
        rty = f77_unit_header_ty();
        if (f77_starts("BLOCKDATA")) {
            /* A declarations-only unit: its DATA lands in the COMMON
             * blocks it names.  It compiles to an empty function that
             * nothing calls; the name is mangled so it cannot collide
             * with a subprogram. */
            if (f77_nunit >= F77_MAX_UNIT) { f77_error("too many program units"); return; }
            f77_upos[f77_nunit] = pos;
            f77_uline[f77_nunit] = line;
            f77_ukind[f77_nunit] = F77_UNIT_BDATA;
            f77_urty[f77_nunit] = TY_INT;
            f77_scan_from(9);
            strcpy(f77_uname[f77_nunit], "f77bd_");
            if (lx_t == T_NAME) strcat(f77_uname[f77_nunit], lex_name);
            {
                char num[12];
                int q;
                int v;
                q = 0; v = f77_nunit;
                num[q++] = '_';
                if (v >= 100) num[q++] = (char)('0' + v / 100);
                if (v >= 10) num[q++] = (char)('0' + (v / 10) % 10);
                num[q++] = (char)('0' + v % 10);
                num[q] = 0;
                strcat(f77_uname[f77_nunit], num);
            }
            f77_nunit = f77_nunit + 1;
            started = 1;
            ended = 0;
        } else if (f77_starts("SUBROUTINE") || rty >= 0) {
            if (f77_nunit >= F77_MAX_UNIT) { f77_error("too many program units"); return; }
            f77_upos[f77_nunit] = pos;
            f77_uline[f77_nunit] = line;
            f77_unit_name(f77_nunit);
            if (hn > 0) {
                f77_uhost[f77_nunit] = hstack[hn - 1];
                f77_unosplice[f77_nunit] = 1;
            }
            f77_collect_dummies(f77_starts("SUBROUTINE") ? 10 : f77_function_skip(), f77_nunit);
            f77_urecur[f77_nunit] = lx_stmt_recursive;
            f77_upure[f77_nunit] = lx_stmt_pure || lx_stmt_elemental;
            f77_uelem[f77_nunit] = lx_stmt_elemental;
            if (lx_stmt_elemental && lx_stmt_recursive) f77_error("an ELEMENTAL procedure is not RECURSIVE");
            strcpy(f77_uresult[f77_nunit], lx_stmt_result);
            if (lx_stmt_result[0] && strcmp(lx_stmt_result, f77_uname[f77_nunit]) == 0)
                f77_error("a FUNCTION's RESULT has a name of its own");
            if (rty >= 0) {
                /* the header is classified many times; its length is
                 * reported once, here */
                if (f77_hdr_len > 0) (void)f77_lenty(rty, f77_hdr_len, 1);
                f77_ukind[f77_nunit] = F77_UNIT_FUNC;
                /* Bare FUNCTION uses the I-N rule; a later type
                 * statement of the function name overrides it. */
                if (f77_starts("FUNCTION"))
                    f77_urty[f77_nunit] = f77_implicit_ty(f77_unit_rname(f77_nunit));
                else
                    f77_urty[f77_nunit] = rty;
                f77_urlog[f77_nunit] = f77_hdr_log;
                if (rty == TY_DERIVED) strcpy(f77_urtn[f77_nunit], f77_hdr_tname);
                f77_urnar[f77_nunit] = f77_starts("FUNCTION") ? 0 : f77_lennar(rty, f77_hdr_len, f77_hdr_log);
                f77_ukname[f77_nunit][0] = 0;
                if (f77_hdr_kind_off >= 0) {
                    /* REAL(8) FUNCTION, REAL(WP): told here when the kind
                     * is constants; a name's value the compilation tells
                     * (f77_emit_copyin), an expression's the header */
                    int kt;
                    f77_scan_begin(f77_nunit);
                    f77_scan_from(f77_hdr_kind_off);
                    kt = f77_scan_kind_sel(rty);
                    if (kt >= 0) f77_urty[f77_nunit] = kt;
                    else f77_urpend[f77_nunit] = 1;
                    /* the name, also where the host's told it: the
                     * unit's USEs may have another */
                    if (!f77_hdr_kind_name(f77_ukname[f77_nunit])) f77_ukname[f77_nunit][0] = 0;
                }
                f77_ultext[f77_nunit][0] = 0;
                if (rty == TY_CHAR && f77_hdr_clen == -2) {
                    /* CHARACTER(LEN=N) FUNCTION: as the kind */
                    f77_scan_begin(f77_nunit);
                    f77_urlen[f77_nunit] = f77_scan_clen(2, f77_nunit, f77_ultext[f77_nunit]);
                } else {
                    f77_urlen[f77_nunit] = (rty == TY_CHAR) ? f77_hdr_clen : 1;
                }
            } else {
                f77_ukind[f77_nunit] = F77_UNIT_SUBR;
                f77_urty[f77_nunit] = TY_INT;
                f77_urlog[f77_nunit] = 0;
                f77_urnar[f77_nunit] = 0;
            }
            f77_nunit = f77_nunit + 1;
            started = 1;
            ended = 0;
        } else if (started && f77_nunit > 0 &&
                   f77_ukind[f77_nunit - 1] == F77_UNIT_FUNC) {
            int ty;
            int skip;
            ty = -1;
            skip = 0;
            int clen;
            int lg;
            int tnar;
            int kpend;
            char clnm[F77_MAX_LTEXT];
            kpend = 0;
            clnm[0] = 0;
            clen = 1;
            lg = 0;
            tnar = 0;
            if (f77_starts("INTEGER")) { ty = TY_INT; skip = 7; }
            else if (f77_starts("LOGICAL")) { ty = TY_INT; skip = 7; lg = 1; }
            else if (f77_starts("DOUBLEPRECISION")) { ty = TY_DOUBLE; skip = 15; }
            else if (f77_starts("DOUBLECOMPLEX")) { ty = TY_DCPLX; skip = 13; }
            else if (f77_starts("REAL")) { ty = TY_FLOAT; skip = 4; }
            else if (f77_starts("CHARACTER")) { ty = TY_CHAR; skip = 9; }
            else if (f77_starts("COMPLEX")) { ty = TY_CPLX; skip = 7; }
            else if (f77_starts("TYPE(")) {
                /* TYPE(name) :: F -- a derived type's result: its name,
                 * for callers to find the type by */
                int e; int i; int dc;
                char tn[F77_MAX_NAME];
                e = f77_match_paren(4);                  /* past its ) */
                if (e - 6 > 0 && e - 6 < F77_MAX_NAME) {
                    memcpy(tn, lx_stmt + 5, (size_t)(e - 6));
                    tn[e - 6] = 0;
                    dc = f77_toplevel(':', 0);
                    i = (dc >= 0 && dc + 1 < lx_stmt_len && lx_stmt[dc + 1] == ':') ? dc + 2 : e;
                    while (i < lx_stmt_len) {
                        char nm[F77_MAX_NAME];
                        int n;
                        n = 0;
                        while (i < lx_stmt_len && (lx_isalpha(lx_stmt[i]) || lx_isdigit(lx_stmt[i]))) { if (n < F77_MAX_NAME - 1) nm[n++] = lx_stmt[i]; i++; }
                        nm[n] = 0;
                        if (n && strcmp(nm, f77_unit_rname(f77_nunit - 1)) == 0) {
                            f77_urty[f77_nunit - 1] = TY_DERIVED;
                            strcpy(f77_urtn[f77_nunit - 1], tn);
                        }
                        while (i < lx_stmt_len && lx_stmt[i] != ',') { if (lx_stmt[i] == '(') { i = f77_match_paren(i); continue; } i++; }
                        i++;
                    }
                }
            }
            if (ty >= 0) {
                int dc;
                int iparam;
                f77_scan_begin(f77_nunit - 1);
                /* INTEGER, PARAMETER :: WP = KIND(1.D0): the scan's table */
                iparam = ty == TY_INT && !lg && f77_has_attr("PARAMETER");
                f77_scan_from(skip);
                if (lx_t == T_LP && ty != TY_CHAR) {
                    /* a kind (Fortran 90): REAL(WP) :: F */
                    int kt;
                    kt = f77_scan_kind_sel(ty);
                    if (kt == -2) kpend = 1;
                    else ty = kt;
                }
                if (lx_t == T_STAR) {
                    int n;
                    if (ty == TY_CHAR) { f77_tok(); clen = f77_scan_clen(0, f77_nunit - 1, clnm); }
                    else if (f77_star_len(&n)) { ty = f77_lenty(ty, n, 0); tnar = f77_lennar(ty, n, lg); }
                    if (lx_t == T_COMMA) f77_tok();
                } else if (lx_t == T_LP && ty == TY_CHAR) {
                    /* CHARACTER(LEN=n), CHARACTER(n) (Fortran 90) */
                    clen = f77_scan_clen(1, f77_nunit - 1, clnm);
                }
                /* Fortran 90: [, attribute]... :: -- the entities follow the :: */
                dc = f77_toplevel(':', 0);
                if (dc >= 0 && dc + 1 < lx_stmt_len && lx_stmt[dc + 1] == ':') f77_scan_from(dc + 2);
                while (lx_t == T_NAME) {
                    int is_fn;
                    char dnm[F77_MAX_NAME];
                    strcpy(dnm, lex_name);
                    is_fn = strcmp(lex_name, f77_unit_rname(f77_nunit - 1)) == 0;
                    f77_tok();
                    if (lx_t == T_STAR && ty == TY_CHAR) {
                        int l;
                        char lnm[F77_MAX_LTEXT];
                        f77_tok();
                        l = f77_scan_clen(0, f77_nunit - 1, lnm);
                        if (is_fn) {
                            f77_urty[f77_nunit - 1] = TY_CHAR;
                            f77_urlen[f77_nunit - 1] = l;
                            strcpy(f77_ultext[f77_nunit - 1], lnm);
                        }
                        else { f77_dn_xlen = l; f77_dn_note(f77_nunit - 1, dnm, TY_CHAR); }
                    } else if (lx_t == T_STAR) {
                        int n;
                        int got;
                        got = f77_star_len(&n);
                        if (got && !is_fn) { f77_dn_xlog = lg; f77_dn_note(f77_nunit - 1, dnm, f77_lenty(ty, n, 0)); }
                        else if (got && is_fn) {
                            f77_urty[f77_nunit - 1] = f77_lenty(ty, n, 0);
                            f77_urlog[f77_nunit - 1] = lg;
                            f77_urnar[f77_nunit - 1] = f77_lennar(ty, n, lg);
                        }
                    } else if (is_fn) {
                        f77_urty[f77_nunit - 1] = ty;
                        f77_urlog[f77_nunit - 1] = lg;
                        f77_urnar[f77_nunit - 1] = tnar;
                        f77_urpend[f77_nunit - 1] = kpend;
                        if (ty == TY_CHAR) { f77_urlen[f77_nunit - 1] = clen; strcpy(f77_ultext[f77_nunit - 1], clnm); }
                    } else {
                        f77_dn_xlog = lg;
                        if (ty == TY_CHAR) f77_dn_xlen = clen;
                        f77_dn_note(f77_nunit - 1, dnm, kpend ? -1 : ty);
                    }
                    if (iparam && lx_t == T_ASSIGN) {
                        f77_tok();
                        f77_scan_mode = 1;
                        f77_scan_bad = 0;
                        f77_cexpr();
                        f77_scan_mode = 0;
                        f77_scan_const_add(f77_nunit - 1, dnm, f77_cx_i, !f77_scan_bad && f77_cx_ty == TY_INT);
                    }
                    if (lx_t == T_LP) {
                        int depth;
                        depth = 1;
                        f77_tok();
                        while (lx_t != T_EOF && depth > 0) {
                            if (lx_t == T_LP) depth = depth + 1;
                            else if (lx_t == T_RP) depth = depth - 1;
                            f77_tok();
                        }
                    }
                    if (lx_t != T_COMMA) break;
                    f77_tok();
                }
            }
        } else if (!started) {
            /* Statements before any subprogram header belong to the
             * main program, whether or not a PROGRAM card is present. */
            f77_upos[f77_nunit] = pos;
            f77_uline[f77_nunit] = line;
            f77_ukind[f77_nunit] = F77_UNIT_PROGRAM;
            f77_urty[f77_nunit] = TY_INT;
            strcpy(f77_uname[f77_nunit], "main");
            f77_nunit = f77_nunit + 1;
            mains = mains + 1;
            started = 1;
        }
        if (started && !ended && !prog[0] && f77_ukind[f77_nunit - 1] == F77_UNIT_PROGRAM
            && f77_starts("PROGRAM") && f77_toplevel('=', 0) < 0) {
            f77_scan_from(7);
            if (lx_t == T_NAME) strcpy(prog, lex_name);
        }
        if (started && f77_is_unit_end()) {
            f77_end_check(f77_nunit - 1, prog);
            ended = 1;
        }
        pos = lx_pos;
        line = lx_line;
    }

    if (hn > 0 || in_iface) { f77_error(in_iface ? "INTERFACE without END INTERFACE" : "CONTAINS without its unit's END"); return; }
    /* Each ENTRY becomes a unit of its own, after the ordinary ones. */
    {
        int q;
        for (q = 0; q < f77_nent; q++) {
            int par;
            if (f77_nunit >= F77_MAX_UNIT) { f77_error("too many program units"); return; }
            par = f77_ent_parent[q];
            f77_upos[f77_nunit] = f77_ent_pos[q];
            f77_uline[f77_nunit] = f77_ent_line[q];
            f77_ukind[f77_nunit] = f77_ukind[par];
            f77_urty[f77_nunit] = (f77_ukind[par] == F77_UNIT_FUNC) ? f77_ent_ty[q] : TY_INT;
            f77_urlog[f77_nunit] = f77_ukind[par] == F77_UNIT_FUNC ? f77_ent_log[q] : 0;
            f77_urnar[f77_nunit] = 0;
            f77_urlen[f77_nunit] = f77_ukind[par] == F77_UNIT_FUNC && f77_ent_ty[q] == TY_CHAR ? f77_ent_len[q] : 1;
            strcpy(f77_uname[f77_nunit], f77_ent_name[q]);
            strcpy(f77_uresult[f77_nunit], f77_ent_res[q]);
            f77_uentry_parent[f77_nunit] = par;
            f77_uhost[f77_nunit] = f77_uhost[par];
            f77_unosplice[f77_nunit] = 1;
            {
                int d;
                for (d = 0; d < f77_nfamd; d++)
                    if (f77_famd_unit[d] == -2 - q) f77_famd_unit[d] = f77_nunit;
            }
            f77_nunit = f77_nunit + 1;
        }
    }
    /* The names they link by: their own, but a module's procedure's
     * MOD.PROC, an internal procedure's HOST.PROC, a module's f77md_MOD. */
    {
        int u;
        for (u = 0; u < f77_nunit; u++) {
            if (f77_ukind[u] == F77_UNIT_MODULE) { strcpy(f77_ulink[u], "f77md_"); strcat(f77_ulink[u], f77_uname[u]); }
            else if (f77_uhost[u] >= 0) {
                strcpy(f77_ulink[u], f77_ulink[f77_uhost[u]][0] && f77_ukind[f77_uhost[u]] != F77_UNIT_MODULE
                                         ? f77_ulink[f77_uhost[u]] : f77_uname[f77_uhost[u]]);
                strcat(f77_ulink[u], ".");
                strcat(f77_ulink[u], f77_uname[u]);
            } else strcpy(f77_ulink[u], f77_uname[u]);
        }
    }
    f77_check_unit_names();
}

/* Phase 2: the backend asks for one unit's HIR. */
static void hl_func(Node *fn) {
    int b_entry;
    int u;

    u = fn->unit;
    hl_cur_fn_dbg = fn->name;
    {
        int q;
        f77_unit_entries = f77_uentry_parent[u] >= 0;
        for (q = 0; q < f77_nent; q++)
            if (f77_ent_parent[q] == u) f77_unit_entries = 1;
    }

    hir_reset();
    f77_ndcxv = 0;                 /* the COMPLEX*16 guard's marks are the unit's */
    hl_nalloca = 0;
    hl_temp_stack = 0;
    f77_nsym = 0;
    f77_nnml = 0;                  /* NAMELIST groups are the unit's */
    f77_nnmlitem = 0;
    f77_nlabel = 0;
    f77_scope_base = 0;
    f77_label_base = 0;
    f77_inline_depth = 0;
    f77_ctl_reset();
    /* Reserve the top 8 bytes of the frame for the saved r31 and r30,
     * exactly as the C compiler's parser does (ps_stack = 8).  Starting
     * at 0 lets the register allocator place the first callee-save slot
     * at fp-4 -- on top of the saved return address -- so any unit with
     * fewer than two locals returned to a wild address.  It only became
     * visible when a bigger .text gave that address somewhere harmful
     * to land. */
    f77_frame = 8;

    b_entry = hir_new_block();
    f77_begin_blk(b_entry);

    /* A host of internal procedures (Fortran 90): the names its
     * procedures' text uses, before its own text is compiled -- those of
     * its variables are host associated, and must be where an internal
     * procedure can reach them (f77_host_table, f77_host_save). */
    f77_host_cands(u);

    /* Rewind to this unit's first statement and bind its header. */
    lx_pos = f77_upos[u];
    lx_line = f77_uline[u];
    if (!f77_next_stmt()) return;
    f77_bind_unit(u);
    fn->nparams = f77_unit_nparams;

    f77_entry_state = 0;
    if (f77_uentry_parent[u] >= 0) {
        /* An ENTRY: the parent's whole text, from its header on.  Its
         * first executable statement branches to the ENTRY statement
         * (f77_entry_state); the statements before that are reached
         * only by a GO TO.  Compiled from the ENTRY onward alone, a
         * GO TO back into them had nowhere to go. */
        int par;
        par = f77_uentry_parent[u];
        f77_entry_line = f77_uline[u];
        f77_entry_blk = hir_new_block();
        f77_entry_state = 1;
        lx_pos = f77_upos[par];
        lx_line = f77_uline[par];
        if (!f77_next_stmt()) return;                 /* the parent's header */
        if (!f77_next_stmt()) { f77_emit_return(); fn->locals_size = f77_frame; return; }
    } else if (f77_ukind[u] != F77_UNIT_PROGRAM) {
        /* The header itself emits nothing; go on to the body. */
        if (!f77_next_stmt()) { f77_emit_return(); fn->locals_size = f77_frame; return; }
    }

    for (;;) {
        f77_statement();
        /* The unit terminator is the statement END, exactly: a prefix
         * match here swallowed everything after the first ENDIF -- the
         * unit ended there, fell off returning 0, and slice2 passed
         * vacuously against the oracle for a full day. */
        /* (an interface body's header and END are no unit's) */
        if ((f77_is_unit_end() && f77_iface_open == -1) || f77_is_contains()) break;
        if (!f77_next_stmt()) break;
        if ((f77_unit_header_ty() >= 0 || f77_starts("SUBROUTINE")) && f77_iface_open == -1) break;
    }
    /* fort370: a block left open at END */
    while (ctl_n > 0) {
        ctl_n = ctl_n - 1;
        if (ctl_kind[ctl_n] == F77_CTL_IF) f77_error("IF block without END IF");
        else if (ctl_kind[ctl_n] == F77_CTL_WHERE) f77_error("WHERE without END WHERE");
        else if (ctl_kind[ctl_n] == F77_CTL_SELECT) f77_error("SELECT CASE without END SELECT");
        else if (ctl_kind[ctl_n] == F77_CTL_FORALL) f77_error("FORALL without END FORALL");
        else if (ctl_label[ctl_n] < 0) f77_error("DO without END DO");
        else {
            /* a labelled DO whose terminal statement never came: no
             * statement carries the label, or it is inside another DO's
             * range (fort370 issue 14) */
            char m[64];
            sprintf(m, "DO %d is never closed by a statement labelled %d", ctl_label[ctl_n], ctl_label[ctl_n]);
            f77_error(m);
        }
    }
    f77_label_check();

    if (f77_cur_blk_live) f77_emit_return();

    /* No block is current once the unit is lowered.  hir_opt mints the
     * constants of its strength reductions with hi_emit, which grows the
     * current block: left on the last one, a shift count landed after
     * the unit's RET, and the CFG a late scalar replacement rebuilds
     * stopped at it -- in a unit with nothing for the first SSA pass to
     * promote, which is the one case hir_ssa_construct does not detach
     * the block itself (ASSOCIATED(P, X(::2)) in a subroutine with no
     * local: "instructions after a block's terminator"). */
    hl_cur_blk = -1;

    /* The HIR is complete: retract the read-only-load assertion for
     * every dummy this unit turned out to store (see f77_ldro_note). */
    f77_ldro_finalize();
    f77_dcx_guard();

    /* Fortran 90: what the unit leaves its procedures -- its implicit
     * typing; a module's table; a host's PARAMETERs and names -- and a
     * module procedure's own entry in its module's table. */
    memcpy(f77_uimp_map[u], f77_imp_map, sizeof(f77_imp_map));
    memcpy(f77_uimp_len[u], f77_imp_len, sizeof(f77_imp_len));
    memcpy(f77_uimp_log[u], f77_imp_log, sizeof(f77_imp_log));
    memcpy(f77_uimp_nar[u], f77_imp_nar, sizeof(f77_imp_nar));
    f77_uimp_none[u] = f77_imp_none;
    if (u == f77_nunit - 1) f77_cblk_finish();     /* the last unit: the blocks are complete */
    if (f77_ukind[u] == F77_UNIT_MODULE) f77_mod_snapshot(u);
    else {
        int v;
        for (v = 0; v < f77_nunit; v++)
            if (f77_uhost[v] == u) { f77_host_save(u); break; }
    }
    if (f77_uhost[u] >= 0 && f77_uentry_parent[u] < 0) {
        if (f77_ukind[f77_uhost[u]] == F77_UNIT_MODULE) {
            /* and its ENTRYs, the module's procedures too: here, where
             * their dummies and results are declared -- the entries
             * themselves are compiled after every other unit, too late
             * for a USE (they were no entity of the module at all) */
            int v;
            f77_mod_proc(u);
            for (v = 0; v < f77_nunit; v++) {
                if (f77_uentry_parent[v] == u) f77_mod_proc(v);
            }
        } else f77_hostnames_check();
    }

    fn->locals_size = f77_frame;
}

int main(int argc, char **argv) {
    static char src[1 << 20];
    static char xsrc[1 << 22];        /* the source, INCLUDEs done */
    int fd;
    int n;
    int xn;

    if (argc < 3) {
        fdputs("usage: f77 source.f output.s\n", 2);
        return 1;
    }

    fd = open(argv[1], 0);
    if (fd < 0) {
        fdputs("f77: cannot open ", 2);
        fdputs(argv[1], 2);
        fdputc('\n', 2);
        return 1;
    }
    n = (int)read(fd, src, sizeof(src) - 1);
    if (n < 0) n = 0;
    src[n] = 0;
    close(fd);

    lx_inname = argv[1];
    lx_kind_hook = f77_kind_lit;
    /* The form and the preprocessing by the name, as gfortran and flang
     * have them: .f90 and .f95 free form, a capital F preprocessed. */
    {
        char *dot;
        dot = strrchr(argv[1], '.');
        if (dot && (strcmp(dot, ".f90") == 0 || strcmp(dot, ".F90") == 0 ||
                    strcmp(dot, ".f95") == 0 || strcmp(dot, ".F95") == 0)) lx_free = 1;
        if (dot && (strcmp(dot, ".F") == 0 || strcmp(dot, ".F90") == 0 || strcmp(dot, ".F95") == 0 ||
                    strcmp(dot, ".FOR") == 0)) lx_cpp = 1;
    }
    if (lx_cpp) {
        lx_cpp_run(src, n);
        if (f77_nerr > 0) return 1;
    }
    xn = 0;
    if (!lx_expand(src, n, -1, xsrc, (int)sizeof(xsrc), &xn)) {
        fdputs("f77: the source with its INCLUDEs is too long\n", 2);
        return 1;
    }
    if (f77_nerr > 0) return 1;
    lx_src = xsrc;
    lx_len = xn;
    lx_pos = 0;
    lx_line = 1;

    f77_scan_units();
    if (f77_nunit == 0) { fdputs("f77: empty source\n", 2); return 1; }

    {
        int i;
        i = 0;
        while (i < f77_nunit) {
            f77_units[i].name = f77_ulink[i];
            f77_units[i].nparams = 0;
            f77_units[i].is_varargs = 0;
            f77_units[i].unit = i;
            f77_units[i].body = NULL;
            f77_units[i].next = (i + 1 < f77_nunit) ? &f77_units[i + 1] : NULL;
            i = i + 1;
        }
    }
    f77_program.body = &f77_units[0];

    /* The output is opened before code generation: the emitter flushes
     * its buffer to it whenever the buffer fills (a full buffer used to
     * be a silent drop). */
    fd = open(argv[2], O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd < 0) {
        fdputs("f77: cannot open output\n", 2);
        return 1;
    }
    cg_fd = fd;
    cg_outpath = argv[2];
    gen_program(&f77_program);
    f77_cblk_comm();
    cg_flush();
    close(fd);
    if (f77_nerr == 0) f77_mod_write();

    if (getenv("HIR_PAIR_STATS")) {
        int z;
        fdputs("split doubles: ", 2);
        fdputuint(2, (unsigned int)f77_split_count);
        fdputc(10, 2);
        {
            int q;
            fdputs("pair-claim [partner,nonode,placed,nofree,HIT]=", 2);
            q = 0;
            while (q < 5) { fdputuint(2, (unsigned)ra_pc_dbg[q]); fdputc(' ', 2); q = q + 1; }
            fdputc(10, 2);
        }
        fdputs("pair-pref hits: ", 2);
        fdputuint(2, (unsigned int)ra_stat_pair_pref);
        fdputs("  addi[calls,spill,nouse,valuse,mismatch,HIT]=", 2);
        z = 0;
        while (z < 6) { fdputuint(2, (unsigned)hcg_dbg_addi[z]); fdputc(' ', 2); z = z + 1; }
        fdputc(10, 2);
    }
    if (f77_nerr > 0) {
        fdputs("f77: compilation failed\n", 2);
        unlink(argv[2]);
        return 1;
    }
    return 0;
}
