/* reader.c -- the ISLISP reader (§6 lexemes, §7 textual representation).
 *
 * Hand-written recursive descent.  The lexeme grammar is six lines, so
 * a scanner generator would cost more than it saves.  The reader works
 * on a string or on a stream through the same cursor so that `read`
 * from a stream and `load` of a file share every rule.
 */
#include "isl.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>

typedef struct {
    const char *src; long len; long pos;
    val stream;             /* when nonzero, read from here instead */
    int hash_pending;       /* skip_ws consumed a '#': the # syntax starts at the next char */
} rd_t;

static int rd_peek(rd_t *r) {
    if (r->stream) return stream_peekc(r->stream);
    if (r->pos >= r->len) return -1;
    return (unsigned char)r->src[r->pos];
}

static int rd_get(rd_t *r) {
    if (r->stream) return stream_getc(r->stream);
    if (r->pos >= r->len) return -1;
    return (unsigned char)r->src[r->pos++];
}

static void rd_unget(rd_t *r, int c) {
    if (c < 0) return;
    if (r->stream) stream_ungetc(r->stream, c);
    else r->pos--;
}

static int is_delim(int c) {
    return c < 0 || isspace(c) || c == '(' || c == ')' || c == '\'' || c == '`' || c == ',' || c == '"' || c == ';';
}

static void parse_error(const char *what) {
    isl_error("<parse-error>", sym_string, mk_string(what, (long)strlen(what)),
              sym_expected_class, SYM(intern_c("<OBJECT>"))->cls, NULL);
}

/* Skip separators and comments; return the next significant char without consuming it. */
static int skip_ws(rd_t *r) {
    int c;
    if (r->hash_pending) return '#' | 0x100;
    for (;;) {
        c = rd_get(r);
        if (c < 0) return -1;
        if (isspace(c)) continue;
        if (c == ';') {
            while ((c = rd_get(r)) >= 0 && c != '\n') ;
            continue;
        }
        if (c == '#') {
            int d = rd_get(r);
            if (d == '|') {
                int depth = 1;
                int prev = 0;
                while (depth > 0) {
                    int e = rd_get(r);
                    if (e < 0) parse_error("unterminated #| comment");
                    if (prev == '|' && e == '#') { depth--; prev = 0; continue; }
                    if (prev == '#' && e == '|') { depth++; prev = 0; continue; }
                    prev = e;
                }
                continue;
            }
            /* '#' has been consumed; push back only what followed it, since a
             * stream holds one pushed-back character.  The caller sees the
             * marker and reads the # syntax from d onwards. */
            rd_unget(r, d);
            r->hash_pending = 1;
            return '#' | 0x100;
        }
        rd_unget(r, c);
        return c;
    }
}

static val read_form(rd_t *r, int *got);

/* A token: everything up to a delimiter, honouring | | and \ escapes.
 * *escaped is set when any escape was seen (then it is a symbol, never a number). */
static long read_token(rd_t *r, char **out, int *escaped) {
    long cap = 64, n = 0;
    char *buf = malloc((size_t)cap);
    int inbar = 0;
    int c;
    *escaped = 0;
    for (;;) {
        c = rd_get(r);
        if (c < 0) break;
        if (inbar) {
            if (c == '|') { inbar = 0; continue; }
            if (c == '\\') { c = rd_get(r); if (c < 0) parse_error("unterminated escape"); }
        } else {
            if (c == '|') { inbar = 1; *escaped = 1; continue; }
            if (c == '\\') { c = rd_get(r); if (c < 0) parse_error("unterminated escape"); *escaped = 1; }
            else if (is_delim(c)) { rd_unget(r, c); break; }
        }
        if (n + 1 >= cap) { cap *= 2; buf = realloc(buf, (size_t)cap); }
        if (!inbar && !*escaped) buf[n++] = (char)c;
        else buf[n++] = (char)c;
    }
    if (inbar) parse_error("unterminated |symbol|");
    buf[n] = 0;
    *out = buf;
    return n;
}

/* Fold the unescaped characters of a token to the neutral case.  The
 * token reader cannot tell escaped from unescaped characters after the
 * fact, so it folds while reading: a simpler rule that is also what the
 * oracle does -- |foo| stays foo, foo becomes FOO, f\oo becomes F\oO? No:
 * an escaped character is kept as written.  So the fold happens here
 * only for tokens that contained no escape at all; a token with escapes
 * is re-read character by character below. */
static val token_to_symbol(rd_t *r_unused, char *buf, long n, int escaped) {
    val s;
    long i;
    (void)r_unused;
    if (!escaped && sym_upper_neutral) {
        for (i = 0; i < n; i++) buf[i] = (char)toupper((unsigned char)buf[i]);
    }
    s = intern(buf, n);
    return s;
}

/* Re-read a token with escapes so that only unescaped letters fold. */
static long read_token_folding(rd_t *r, char **out) {
    long cap = 64, n = 0;
    char *buf = malloc((size_t)cap);
    int inbar = 0;
    int c;
    for (;;) {
        c = rd_get(r);
        if (c < 0) break;
        if (inbar) {
            if (c == '|') { inbar = 0; continue; }
            if (c == '\\') { c = rd_get(r); if (c < 0) parse_error("unterminated escape"); }
        } else {
            if (c == '|') { inbar = 1; continue; }
            if (c == '\\') { c = rd_get(r); if (c < 0) parse_error("unterminated escape"); }
            else if (is_delim(c)) { rd_unget(r, c); break; }
            else if (sym_upper_neutral) c = toupper(c);
        }
        if (n + 1 >= cap) { cap *= 2; buf = realloc(buf, (size_t)cap); }
        buf[n++] = (char)c;
    }
    buf[n] = 0;
    *out = buf;
    return n;
}

static int is_digit_radix(int c, int radix) {
    int v;
    if (c >= '0' && c <= '9') v = c - '0';
    else if (c >= 'a' && c <= 'z') v = c - 'a' + 10;
    else if (c >= 'A' && c <= 'Z') v = c - 'A' + 10;
    else return 0;
    return v < radix;
}

/* §7: integer [s]d+ ; float [s]d+.d+ | [s]d+.d+E[s]d+ | [s]d+E[s]d+ (E or e). */
int parse_number_text(const char *s, long len, val *out) {
    long i = 0;
    int neg = 0;
    long int_start, int_end;
    int has_dot = 0, has_exp = 0;
    if (len == 0) return 0;
    if (s[i] == '+' || s[i] == '-') { neg = s[i] == '-'; i++; }
    int_start = i;
    while (i < len && isdigit((unsigned char)s[i])) i++;
    int_end = i;
    if (int_end == int_start) return 0;
    if (i < len && s[i] == '.') {
        long fs;
        has_dot = 1;
        i++;
        fs = i;
        while (i < len && isdigit((unsigned char)s[i])) i++;
        if (i == fs) return 0;            /* "1." is not a number */
    }
    if (i < len && (s[i] == 'e' || s[i] == 'E')) {
        long es;
        has_exp = 1;
        i++;
        if (i < len && (s[i] == '+' || s[i] == '-')) i++;
        es = i;
        while (i < len && isdigit((unsigned char)s[i])) i++;
        if (i == es) return 0;
    }
    if (i != len) return 0;
    if (!has_dot && !has_exp) {
        int ok;
        val v = parse_integer_text(s + int_start, int_end - int_start, 10, &ok);
        if (!ok) return 0;
        if (neg) v = int_neg(v);
        *out = v;
        return 1;
    }
    {
        char *tmp = malloc((size_t)len + 1);
        double d;
        memcpy(tmp, s, (size_t)len);
        tmp[len] = 0;
        d = strtod(tmp, NULL);
        free(tmp);
        if (d == HUGE_VAL || d == -HUGE_VAL) err_float_overflow(intern_c("PARSE-NUMBER"), list1(mk_string(s, len)));
        if (d != 0.0 && (d < 0 ? -d : d) < 2.2250738585072014e-308) err_float_underflow(intern_c("PARSE-NUMBER"), list1(mk_string(s, len)));
        if (d == 0.0) {
            /* a zero from non-zero digits: 1.0e-1000 */
            long k = 0;
            int nonzero = 0;
            while (k < len && s[k] != 'e' && s[k] != 'E') { if (s[k] >= '1' && s[k] <= '9') nonzero = 1; k++; }
            if (nonzero) err_float_underflow(intern_c("PARSE-NUMBER"), list1(mk_string(s, len)));
        }
        *out = mk_float(d);
        return 1;
    }
}

static val read_list(rd_t *r) {
    val head = NIL, tail = NIL;
    long sp = gc_sp;
    int c;
    gc_push(NIL);               /* slot for head */
    for (;;) {
        c = skip_ws(r);
        if (c < 0) parse_error("unterminated list");
        if (c == ')') { rd_get(r); break; }
        if (c == '.' ) {
            /* a lone dot is the dotted-pair marker */
            int d;
            rd_get(r);
            d = rd_peek(r);
            if (is_delim(d)) {
                int got;
                val last;
                if (head == NIL) parse_error("dot at start of list");
                last = read_form(r, &got);
                if (!got) parse_error("dotted pair without cdr");
                CDR(tail) = last;
                c = skip_ws(r);
                if (c != ')') parse_error("dotted pair not closed");
                rd_get(r);
                break;
            }
            rd_unget(r, '.');
        }
        {
            int got;
            val item = read_form(r, &got);
            val cell;
            if (!got) parse_error("unterminated list");
            cell = mk_cons(item, NIL);
            if (head == NIL) { head = cell; gc_roots[sp] = head; }
            else CDR(tail) = cell;
            tail = cell;
        }
    }
    gc_sp = sp;
    return head;
}

static val read_string(rd_t *r) {
    long cap = 64, n = 0;
    char *buf = malloc((size_t)cap);
    int c;
    val s;
    for (;;) {
        c = rd_get(r);
        if (c < 0) parse_error("unterminated string");
        if (c == '"') break;
        if (c == '\\') { c = rd_get(r); if (c < 0) parse_error("unterminated string"); }
        if (n + 1 >= cap) { cap *= 2; buf = realloc(buf, (size_t)cap); }
        buf[n++] = (char)c;
    }
    s = mk_string(buf, n);
    free(buf);
    return s;
}

static val read_character(rd_t *r) {
    int c = rd_get(r);
    char name[32];
    int n = 0;
    if (c < 0) parse_error("unterminated character");
    name[n++] = (char)c;
    /* a named character: letters follow the first one */
    for (;;) {
        int d = rd_peek(r);
        if (is_delim(d)) break;
        if (n < 30) name[n++] = (char)d;
        rd_get(r);
    }
    name[n] = 0;
    if (n == 1) return MKCHR((unsigned char)name[0]);
    if (strcasecmp(name, "space") == 0) return MKCHR(' ');
    if (strcasecmp(name, "newline") == 0) return MKCHR('\n');
    if (strcasecmp(name, "tab") == 0) return MKCHR('\t');
    if (strcasecmp(name, "return") == 0) return MKCHR('\r');
    if (strcasecmp(name, "null") == 0) return MKCHR(0);
    parse_error("unknown character name");
    return NIL;
}

/* #na(...) : build a general array of the given rank from nested lists. */
static long array_count_leaves(val nested, int rank, val dims, int depth) {
    long n;
    if (depth == rank) return 1;
    n = list_length(nested);
    if (n < 0) parse_error("malformed #na array");
    if (VEC(dims)->elt[depth] == UNBOUND) VEC(dims)->elt[depth] = MKFIX(n);
    else if (FIXVAL(VEC(dims)->elt[depth]) != n) parse_error("ragged #na array");
    {
        long total = 0;
        val l;
        for (l = nested; IS_CONS(l); l = CDR(l)) total += array_count_leaves(CAR(l), rank, dims, depth + 1);
        return total;
    }
}
static void array_fill(val nested, int rank, int depth, val data, long *idx) {
    if (depth == rank) { VEC(data)->elt[(*idx)++] = nested; return; }
    for (; IS_CONS(nested); nested = CDR(nested)) array_fill(CAR(nested), rank, depth + 1, data, idx);
}

val mk_array_from_nested(int rank, val nested) {
    val dims, data;
    array_t *a;
    long total, idx = 0, i;
    long sp = gc_sp;
    gc_push(nested);
    dims = mk_vector(rank, UNBOUND);
    gc_push(dims);
    total = rank == 0 ? 1 : array_count_leaves(nested, rank, dims, 0);
    for (i = 0; i < rank; i++) if (VEC(dims)->elt[i] == UNBOUND) VEC(dims)->elt[i] = MKFIX(0);
    data = mk_vector(total, NIL);
    gc_push(data);
    if (rank == 0) VEC(data)->elt[0] = nested;
    else array_fill(nested, rank, 0, data, &idx);
    if (rank == 1) { gc_sp = sp; return data; }   /* #1a(...) is a general vector */
    a = (array_t *)heap_alloc(T_ARRAY, sizeof(array_t));
    a->rank = rank; a->dims = dims; a->data = data;
    gc_sp = sp;
    return (val)a;
}

static val read_hash(rd_t *r) {
    int c = rd_get(r);
    if (c == '(') {
        val l = read_list(r);
        long n = list_length(l), i;
        val v;
        long sp = gc_sp;
        gc_push(l);
        v = mk_vector(n, NIL);
        for (i = 0; i < n; i++, l = CDR(l)) VEC(v)->elt[i] = CAR(l);
        gc_sp = sp;
        return v;
    }
    if (c == '\'') {
        int got;
        val f = read_form(r, &got);
        if (!got) parse_error("#' without a function name");
        return list2(sym_function, f);
    }
    if (c == '\\') return read_character(r);
    if (c == 'b' || c == 'B' || c == 'o' || c == 'O' || c == 'x' || c == 'X') {
        int radix = (c == 'b' || c == 'B') ? 2 : (c == 'o' || c == 'O') ? 8 : 16;
        char *buf;
        int esc;
        long n;
        int ok, neg = 0;
        long i = 0;
        val v;
        n = read_token(r, &buf, &esc);
        if (n > 0 && (buf[0] == '+' || buf[0] == '-')) { neg = buf[0] == '-'; i = 1; }
        if (i >= n) parse_error("malformed radix integer");
        for (; i < n; i++) if (!is_digit_radix((unsigned char)buf[i], radix)) parse_error("malformed radix integer");
        i = (n > 0 && (buf[0] == '+' || buf[0] == '-')) ? 1 : 0;
        v = parse_integer_text(buf + i, n - i, radix, &ok);
        free(buf);
        if (neg) v = int_neg(v);
        return v;
    }
    if (isdigit(c)) {
        int rank = 0;
        while (isdigit(c)) { rank = rank * 10 + (c - '0'); c = rd_get(r); }
        if (c != 'a' && c != 'A') parse_error("malformed #na");
        if (rank == 0) {
            int got;
            val item = read_form(r, &got);
            if (!got) parse_error("#0a needs an object");
            return mk_array_from_nested(0, item);
        }
        c = skip_ws(r);
        if (c != '(') parse_error("#na needs a list");
        rd_get(r);
        {
            val nested = read_list(r);
            return mk_array_from_nested(rank, nested);
        }
    }
    parse_error("unknown # syntax");
    return NIL;
}

static val read_form(rd_t *r, int *got) {
    int c = skip_ws(r);
    *got = 1;
    if (c < 0) { *got = 0; return NIL; }
    if (c == ('#' | 0x100)) {
        r->hash_pending = 0;
        return read_hash(r);
    }
    rd_get(r);
    switch (c) {
    case '(':
        return read_list(r);
    case ')':
        parse_error("unexpected )");
        return NIL;
    case '\'': {
        val f = read_form(r, got);
        if (!*got) parse_error("quote without a form");
        return list2(sym_quote, f);
    }
    case '`': {
        val f = read_form(r, got);
        if (!*got) parse_error("quasiquote without a form");
        return list2(sym_quasiquote, f);
    }
    case ',': {
        int d = rd_peek(r);
        val f;
        if (d == '@') {
            rd_get(r);
            f = read_form(r, got);
            if (!*got) parse_error(",@ without a form");
            return list2(sym_splice, f);
        }
        f = read_form(r, got);
        if (!*got) parse_error(", without a form");
        return list2(sym_unquote, f);
    }
    case '"':
        return read_string(r);
    case '#':
        return read_hash(r);
    default: {
        char *buf;
        long n;
        val v;
        int esc;
        rd_unget(r, c);
        /* Peek for escapes so the folding reader is used only when needed. */
        n = read_token_folding(r, &buf);
        esc = 0;
        if (n == 0) { free(buf); parse_error("empty token"); }
        if (!esc && parse_number_text(buf, n, &v)) { free(buf); return v; }
        v = token_to_symbol(r, buf, n, 1);   /* already folded */
        free(buf);
        return v;
    }
    }
}

val read_from_string(const char *s, long len, long *consumed, int *got) {
    rd_t r;
    val v;
    r.src = s; r.len = len; r.pos = 0; r.stream = 0; r.hash_pending = 0;
    v = read_form(&r, got);
    if (consumed) *consumed = r.pos;
    return v;
}

val read_from_stream(val stream, int *got) {
    rd_t r;
    r.src = 0; r.len = 0; r.pos = 0; r.stream = stream; r.hash_pending = 0;
    return read_form(&r, got);
}
