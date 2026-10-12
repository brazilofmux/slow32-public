/* printer.c -- textual representation on output (§7).
 *
 * Two modes, as format's ~A and ~S: with escape, strings carry their
 * quotes and backslashes and characters their #\, and a symbol whose
 * name would not read back is written between vertical bars; without,
 * everything is written as plain text.  Floats print the shortest
 * digit string that reads back to the same double, in the §7 syntax
 * (a digit on both sides of the point; an exponent when the magnitude
 * is far from one).  nil prints as NIL, the oracle's choice where §7
 * leaves it open.
 */
#include "isl.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>

void stream_putc(val stream, int c) {
    stream_write(stream, (const char[]){ (char)c }, 1);
}

void stream_puts(val stream, const char *s) {
    stream_write(stream, s, (long)strlen(s));
}

/* Shortest round-trip, then reshaped into the standard's float syntax. */
void float_to_text(double d, char *buf) {
    char tmp[40];
    int prec;
    if (d != d) { strcpy(buf, "NaN"); return; }
    if (d == HUGE_VAL) { strcpy(buf, "+Inf"); return; }
    if (d == -HUGE_VAL) { strcpy(buf, "-Inf"); return; }
    for (prec = 1; prec <= 17; prec++) {
        snprintf(tmp, sizeof tmp, "%.*e", prec - 1, d);
        if (strtod(tmp, NULL) == d) break;
    }
    /* tmp is [-]d.ddde[+-]xx ; rebuild as the shortest of fixed / exponent forms */
    {
        char digits[32];
        int nd = 0, exp10, neg = 0;
        char *p = tmp;
        int i;
        if (*p == '-') { neg = 1; p++; }
        for (; *p && *p != 'e'; p++) if (isdigit((unsigned char)*p)) digits[nd++] = *p;
        digits[nd] = 0;
        exp10 = atoi(p + 1);       /* value = 0.d1d2.. * 10^(exp10+1) */
        while (nd > 1 && digits[nd - 1] == '0') nd--;
        digits[nd] = 0;
        p = buf;
        if (neg) *p++ = '-';
        if (exp10 >= -4 && exp10 < 16) {
            if (exp10 < 0) {
                *p++ = '0'; *p++ = '.';
                for (i = 0; i < -exp10 - 1; i++) *p++ = '0';
                memcpy(p, digits, (size_t)nd); p += nd;
            } else {
                int intdig = exp10 + 1;
                for (i = 0; i < intdig; i++) *p++ = i < nd ? digits[i] : '0';
                *p++ = '.';
                if (nd > intdig) { memcpy(p, digits + intdig, (size_t)(nd - intdig)); p += nd - intdig; }
                else *p++ = '0';
            }
            *p = 0;
        } else {
            *p++ = digits[0];
            *p++ = '.';
            if (nd > 1) { memcpy(p, digits + 1, (size_t)(nd - 1)); p += nd - 1; }
            else *p++ = '0';
            sprintf(p, "e%d", exp10);
        }
    }
}

static int symbol_needs_bars(const char *s, long n) {
    long i;
    if (n == 0) return 1;
    for (i = 0; i < n; i++) {
        unsigned char c = (unsigned char)s[i];
        if (isalpha(c)) { if (sym_upper_neutral ? islower(c) : isupper(c)) return 1; continue; }
        if (isdigit(c)) continue;
        if (strchr("+-<>/*=?_!$%[]^{}~.:&@", c)) continue;
        return 1;
    }
    /* a name that would read as a number needs bars too */
    {
        val dummy;
        if (parse_number_text(s, n, &dummy)) return 1;
    }
    return 0;
}

static void print_symbol(val stream, val s, int escape) {
    string_t *name = STR(SYM(s)->name);
    if (!escape || !symbol_needs_bars(name->data, name->len)) {
        stream_write(stream, name->data, name->len);
        return;
    }
    stream_putc(stream, '|');
    {
        long i;
        for (i = 0; i < name->len; i++) {
            if (name->data[i] == '|' || name->data[i] == '\\') stream_putc(stream, '\\');
            stream_putc(stream, name->data[i]);
        }
    }
    stream_putc(stream, '|');
}

static void print_rec(val stream, val v, int escape, int depth);

static void print_array_level(val stream, val a, int level, long *idx, int escape, int depth) {
    long n = FIXVAL(VEC(ARR(a)->dims)->elt[level]);
    long i;
    stream_putc(stream, '(');
    for (i = 0; i < n; i++) {
        if (i) stream_putc(stream, ' ');
        if (level + 1 == ARR(a)->rank) print_rec(stream, VEC(ARR(a)->data)->elt[(*idx)++], escape, depth + 1);
        else print_array_level(stream, a, level + 1, idx, escape, depth);
    }
    stream_putc(stream, ')');
}

static void print_rec(val stream, val v, int escape, int depth) {
    char buf[64];
    if (depth > 10000) { stream_puts(stream, "..."); return; }
    if (IS_FIX(v)) { snprintf(buf, sizeof buf, "%ld", FIXVAL(v)); stream_puts(stream, buf); return; }
    if (IS_CHR(v)) {
        int c = CHRVAL(v);
        if (!escape) { stream_putc(stream, c); return; }
        if (c == ' ') stream_puts(stream, "#\\space");
        else if (c == '\n') stream_puts(stream, "#\\newline");
        else if (c == '\t') stream_puts(stream, "#\\tab");
        else if (c == '\r') stream_puts(stream, "#\\return");
        else if (c == 0) stream_puts(stream, "#\\null");
        else { stream_puts(stream, "#\\"); stream_putc(stream, c); }
        return;
    }
    if (!IS_PTR(v)) { stream_puts(stream, "#<bad>"); return; }
    switch (TAG(v)) {
    case T_SYMBOL: print_symbol(stream, v, escape); return;
    case T_STRING:
        if (!escape) { stream_write(stream, STR(v)->data, STR(v)->len); return; }
        stream_putc(stream, '"');
        {
            long i;
            for (i = 0; i < STR(v)->len; i++) {
                char c = STR(v)->data[i];
                if (c == '"' || c == '\\') stream_putc(stream, '\\');
                stream_putc(stream, c);
            }
        }
        stream_putc(stream, '"');
        return;
    case T_FLOAT: float_to_text(FLT(v)->d, buf); stream_puts(stream, buf); return;
    case T_BIGNUM: {
        char *txt; long n;
        int_to_text(v, 10, &txt, &n);
        stream_write(stream, txt, n);
        free(txt);
        return;
    }
    case T_CONS: {
        long n = 0;
        if (CAR(v) == sym_quote && IS_CONS(CDR(v)) && CDR(CDR(v)) == NIL) {
            stream_putc(stream, '\'');
            print_rec(stream, CAR(CDR(v)), escape, depth + 1);
            return;
        }
        stream_putc(stream, '(');
        for (;;) {
            print_rec(stream, CAR(v), escape, depth + 1);
            v = CDR(v);
            if (v == NIL) break;
            if (!IS_CONS(v) || ++n > 100000) { stream_puts(stream, " . "); print_rec(stream, v, escape, depth + 1); break; }
            stream_putc(stream, ' ');
        }
        stream_putc(stream, ')');
        return;
    }
    case T_VECTOR: {
        long i;
        stream_puts(stream, "#(");
        for (i = 0; i < VEC(v)->n; i++) {
            if (i) stream_putc(stream, ' ');
            print_rec(stream, VEC(v)->elt[i], escape, depth + 1);
        }
        stream_putc(stream, ')');
        return;
    }
    case T_ARRAY: {
        long idx = 0;
        snprintf(buf, sizeof buf, "#%da", ARR(v)->rank);
        stream_puts(stream, buf);
        if (ARR(v)->rank == 0) print_rec(stream, VEC(ARR(v)->data)->elt[0], escape, depth + 1);
        else print_array_level(stream, v, 0, &idx, escape, depth);
        return;
    }
    case T_CLOSURE:
        stream_puts(stream, "#<function");
        if (CLO(v)->name != NIL) { stream_putc(stream, ' '); print_symbol(stream, CLO(v)->name, 0); }
        stream_putc(stream, '>');
        return;
    case T_BUILTIN: stream_puts(stream, "#<function "); stream_puts(stream, BI(v)->name); stream_putc(stream, '>'); return;
    case T_MACRO: stream_puts(stream, "#<macro "); print_symbol(stream, MAC(v)->name, 0); stream_putc(stream, '>'); return;
    case T_GENERIC: stream_puts(stream, "#<generic-function "); print_symbol(stream, GEN(v)->name, 0); stream_putc(stream, '>'); return;
    case T_METHOD: stream_puts(stream, "#<method>"); return;
    case T_CLASS: {
        /* no textual representation in the text (§7); the oracle's shape, for the byte diff */
        string_t *nm = STR(SYM(CLS(v)->name)->name);
        long i;
        stream_puts(stream, "<class ");
        for (i = 0; i < nm->len; i++) {
            char c = nm->data[i];
            if (c == '<' || c == '>') continue;
            stream_putc(stream, (c >= 'A' && c <= 'Z') ? c + 32 : c);
        }
        stream_putc(stream, '>');
        return;
    }
    case T_INSTANCE: stream_puts(stream, "#<instance of "); print_symbol(stream, CLS(INST(v)->cls)->name, 0); stream_putc(stream, '>'); return;
    case T_STREAM: stream_puts(stream, "#<stream>"); return;
    case T_ENV: stream_puts(stream, "#<environment>"); return;
    default: stream_puts(stream, "#<?>"); return;
    }
}

void print_to_stream(val stream, val v, int escape) {
    print_rec(stream, v, escape, 0);
}

val print_to_string(val v, int escape) {
    val s = mk_string_output_stream();
    long sp = gc_sp;
    gc_push(s);
    print_rec(s, v, escape, 0);
    gc_sp = sp;
    return string_output_contents(s);
}
