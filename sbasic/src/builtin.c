#include "builtin.h"
#include "eval.h"
#include "fileio.h"
#include "terminal.h"
#include "graphics.h"
#include "machine.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <time.h>
#include <dirent.h>
#include <unistd.h>
#include <slow32_args.h>

/* QuickBASIC's generator, so a program's random numbers are QBasic's: a
 * 24-bit linear congruence from 0x50000, RND = seed / 2^24 (QBasic 1.1's
 * first values .7055475, .533424, .5795186 checked) */
static unsigned int rnd_seed = 0x50000;

static double sb_rnd(void) {
    rnd_seed = (rnd_seed * 16598013u + 12820163u) & 0xFFFFFF;
    return rnd_seed / 16777216.0;
}

/* ================================================================
 * Helper: extract numeric argument
 * ================================================================ */
static error_t get_num(value_t *v, double *out) {
    if (v->type == VAL_STRING) return ERR_TYPE_MISMATCH;
    if (v->type == VAL_RECORD) return ERR_TYPE_MISMATCH;
    *out = VAL_IS_INT(v->type) ? (double)v->ival : v->dval;
    return ERR_NONE;
}

/* ================================================================
 * Math built-in functions
 * ================================================================ */

static error_t fn_abs(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type == VAL_STRING) return ERR_TYPE_MISMATCH;
    /* the argument's type kept: ABS(-32768) is an Overflow */
    if (VAL_IS_INT(args[0].type)) {
        long long v = args[0].ival;
        if (v < 0) v = -v;
        if (args[0].type == VAL_INTEGER ? !INT16_OK(v) : v > 2147483647LL)
            return ERR_OVERFLOW;
        *out = args[0];
        out->ival = (int)v;
    } else {
        *out = args[0];
        out->dval = fabs(args[0].dval);
    }
    return ERR_NONE;
}

static error_t fn_int(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    /* INT and FIX keep their argument's type (INT(1E+10) is a single) */
    if (VAL_IS_INT(args[0].type)) *out = args[0];
    else { *out = args[0]; out->dval = floor(v); }
    return ERR_NONE;
}

static error_t fn_sgn(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    if (v > 0) *out = val_integer(1);
    else if (v < 0) *out = val_integer(-1);
    else *out = val_integer(0);
    return ERR_NONE;
}

static error_t fn_sqr(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    if (v < 0) return ERR_ILLEGAL_FUNCTION_CALL;
    *out = val_double(sqrt(v));
    return ERR_NONE;
}

static error_t fn_fix(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    /* truncated toward zero, the argument's type kept */
    if (VAL_IS_INT(args[0].type)) *out = args[0];
    else { *out = args[0]; out->dval = trunc(v); }
    return ERR_NONE;
}

static error_t fn_cint(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    /* to the nearest integer, a half to even (QuickBASIC: CINT(2.5) = 2) */
    double rounded = val_rint(v);
    if (!INT16_OK(rounded))
        return ERR_OVERFLOW;
    *out = val_integer((int)rounded);
    return ERR_NONE;
}

static error_t fn_clng(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    double rounded = val_rint(v);
    if (rounded > 2147483647.0 || rounded < -2147483648.0)
        return ERR_OVERFLOW;
    *out = val_long((int)rounded);
    return ERR_NONE;
}

static error_t fn_cdbl(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(v);
    return ERR_NONE;
}

static error_t fn_csng(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_single(single_round(v));
    return ERR_NONE;
}

/* Trig functions */
static error_t fn_sin(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(sin(v));
    return ERR_NONE;
}

static error_t fn_cos(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(cos(v));
    return ERR_NONE;
}

static error_t fn_tan(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(tan(v));
    return ERR_NONE;
}

static error_t fn_atn(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(atan(v));
    return ERR_NONE;
}

static error_t fn_atn2(value_t *args, int nargs, value_t *out) {
    if (nargs != 2) return ERR_ILLEGAL_FUNCTION_CALL;
    double y, x;
    EVAL_CHECK(get_num(&args[0], &y));
    EVAL_CHECK(get_num(&args[1], &x));
    *out = val_double(atan2(y, x));
    return ERR_NONE;
}

static error_t fn_asin(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(asin(v));
    return ERR_NONE;
}

static error_t fn_acos(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(acos(v));
    return ERR_NONE;
}

/* Hyperbolic functions */
static error_t fn_sinh(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(sinh(v));
    return ERR_NONE;
}

static error_t fn_cosh(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(cosh(v));
    return ERR_NONE;
}

static error_t fn_tanh(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(tanh(v));
    return ERR_NONE;
}

/* Inverse hyperbolic functions */
static error_t fn_asinh(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(log(v + sqrt(v * v + 1.0)));
    return ERR_NONE;
}

static error_t fn_acosh(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    if (v < 1.0) return ERR_ILLEGAL_FUNCTION_CALL;
    *out = val_double(log(v + sqrt(v * v - 1.0)));
    return ERR_NONE;
}

static error_t fn_atanh(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    if (v <= -1.0 || v >= 1.0) return ERR_ILLEGAL_FUNCTION_CALL;
    *out = val_double(0.5 * log((1.0 + v) / (1.0 - v)));
    return ERR_NONE;
}

static error_t fn_exp(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(exp(v));
    return ERR_NONE;
}

static error_t fn_log(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    if (v <= 0.0) return ERR_ILLEGAL_FUNCTION_CALL;
    *out = val_double(log(v));
    return ERR_NONE;
}

static error_t fn_log10(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    if (v <= 0.0) return ERR_ILLEGAL_FUNCTION_CALL;
    *out = val_double(log10(v));
    return ERR_NONE;
}

static error_t fn_log2(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    if (v <= 0.0) return ERR_ILLEGAL_FUNCTION_CALL;
    *out = val_double(log2(v));
    return ERR_NONE;
}

static error_t fn_round(value_t *args, int nargs, value_t *out) {
    if (nargs < 1 || nargs > 2) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    int decimals = 0;
    if (nargs == 2) {
        EVAL_CHECK(val_to_integer(&args[1], &decimals));
    }
    if (decimals == 0) {
        *out = val_double(floor(v + 0.5));
    } else {
        double mul = 1.0;
        int d = decimals < 0 ? -decimals : decimals;
        for (int i = 0; i < d; i++) mul *= 10.0;
        if (decimals > 0)
            *out = val_double(floor(v * mul + 0.5) / mul);
        else
            *out = val_double(floor(v / mul + 0.5) * mul);
    }
    return ERR_NONE;
}

/* RND, RND(n): n > 0 the next number, 0 the last one again, n < 0 a
 * new seed from the bits of n as a single, then the next */
/* RND(n) of a number (has_n 0: RND alone), the value of the single it
 * makes; for the compiler too */
double sb_rnd_of(double n, int has_n) {
    if (has_n) {
        if (n == 0.0)
            return rnd_seed / 16777216.0;
        if (n < 0.0) {
            union { float f; unsigned int u; } b;
            b.f = (float)n;
            rnd_seed = ((b.u & 0xFFFFFF) + (b.u >> 24)) & 0xFFFFFF;
        }
    }
    return sb_rnd();
}

static error_t fn_rnd(value_t *args, int nargs, value_t *out) {
    if (nargs > 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double n = 0.0;
    if (nargs == 1) EVAL_CHECK(get_num(&args[0], &n));
    *out = val_single(sb_rnd_of(n, nargs == 1));
    return ERR_NONE;
}

/* ================================================================
 * String built-in functions
 * ================================================================ */

static error_t fn_len(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    *out = val_integer(args[0].sval->len);
    return ERR_NONE;
}

error_t sb_chr(int code, value_t *out) {
    if (code < 0 || code > 255) return ERR_ILLEGAL_FUNCTION_CALL;
    out->type = VAL_STRING;
    out->sval = string_char(code);
    return ERR_NONE;
}

static error_t fn_chr(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int code;
    EVAL_CHECK(val_to_integer(&args[0], &code));
    return sb_chr(code, out);
}

static error_t fn_asc(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    if (args[0].sval->len == 0) return ERR_ILLEGAL_FUNCTION_CALL;
    *out = val_integer((unsigned char)args[0].sval->data[0]);
    return ERR_NONE;
}

static error_t fn_str(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    char buf[64];
    if (args[0].type == VAL_STRING || args[0].type == VAL_RECORD)
        return ERR_TYPE_MISMATCH;
    EVAL_CHECK(val_check_range(&args[0]));
    /* as PRINT writes it, with the sign's place: STR$(5) is " 5" */
    buf[0] = ' ';
    val_format_number(&args[0], buf + 1, sizeof(buf) - 1);
    *out = val_string_cstr(buf[1] == '-' ? buf + 1 : buf);
    return ERR_NONE;
}

static error_t fn_val(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    const char *s = args[0].sval->data;
    /* Skip leading whitespace */
    while (*s == ' ' || *s == '\t') s++;
    /* Check for &H (hex), &O (octal), &B (binary) prefixes */
    if (s[0] == '&' && s[1]) {
        char prefix = toupper((unsigned char)s[1]);
        if (prefix == 'H') {
            *out = val_integer((int)strtol(s + 2, NULL, 16));
            return ERR_NONE;
        } else if (prefix == 'O') {
            *out = val_integer((int)strtol(s + 2, NULL, 8));
            return ERR_NONE;
        } else if (prefix == 'B') {
            *out = val_integer((int)strtol(s + 2, NULL, 2));
            return ERR_NONE;
        }
    }
    char *endp;
    long lv = strtol(s, &endp, 10);
    if (*endp == '\0' || *endp == ' ') {
        *out = val_integer((int)lv);
    } else {
        *out = val_double(atof(s));
    }
    return ERR_NONE;
}

/* LEFT$, RIGHT$ and MID$ of a string and its integer arguments, for
 * the compiler too (count < 0 with has_count 0: MID$ with two) */
error_t sb_left(sb_string_t *s, int n, value_t *out) {
    if (n < 0) return ERR_ILLEGAL_FUNCTION_CALL;
    if (n > s->len) n = s->len;
    out->type = VAL_STRING;
    out->sval = string_sub(s, 0, n);
    return ERR_NONE;
}

error_t sb_right(sb_string_t *s, int n, value_t *out) {
    if (n < 0) return ERR_ILLEGAL_FUNCTION_CALL;
    if (n > s->len) n = s->len;
    out->type = VAL_STRING;
    out->sval = string_sub(s, s->len - n, n);
    return ERR_NONE;
}

error_t sb_mid(sb_string_t *s, int start, int count, int has_count, value_t *out) {
    start--;
    if (start < 0) start = 0;
    int len = s->len;
    if (!has_count) count = len - start;
    else if (count < 0) return ERR_ILLEGAL_FUNCTION_CALL;
    out->type = VAL_STRING;
    if (start >= len) {
        out->sval = string_empty();
        return ERR_NONE;
    }
    if (count > len - start) count = len - start;
    out->sval = string_sub(s, start, count);
    return ERR_NONE;
}

static error_t fn_left(value_t *args, int nargs, value_t *out) {
    if (nargs != 2) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    int n;
    EVAL_CHECK(val_to_integer(&args[1], &n));
    return sb_left(args[0].sval, n, out);
}

static error_t fn_right(value_t *args, int nargs, value_t *out) {
    if (nargs != 2) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    int n;
    EVAL_CHECK(val_to_integer(&args[1], &n));
    return sb_right(args[0].sval, n, out);
}

static error_t fn_mid(value_t *args, int nargs, value_t *out) {
    if (nargs < 2 || nargs > 3) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    int start, count = 0;
    EVAL_CHECK(val_to_integer(&args[1], &start));
    if (nargs == 3) EVAL_CHECK(val_to_integer(&args[2], &count));
    return sb_mid(args[0].sval, start, count, nargs == 3, out);
}

/* INSTR([start,] str, search) - find substring */
static error_t fn_instr(value_t *args, int nargs, value_t *out) {
    int start = 1;
    const char *haystack, *needle;
    int hlen;
    if (nargs == 2) {
        if (args[0].type != VAL_STRING || args[1].type != VAL_STRING)
            return ERR_TYPE_MISMATCH;
        haystack = args[0].sval->data;
        hlen = args[0].sval->len;
        needle = args[1].sval->data;
    } else if (nargs == 3) {
        EVAL_CHECK(val_to_integer(&args[0], &start));
        if (args[1].type != VAL_STRING || args[2].type != VAL_STRING)
            return ERR_TYPE_MISMATCH;
        haystack = args[1].sval->data;
        hlen = args[1].sval->len;
        needle = args[2].sval->data;
    } else {
        return ERR_ILLEGAL_FUNCTION_CALL;
    }
    if (start < 1) return ERR_ILLEGAL_FUNCTION_CALL;
    start--;
    if (start >= hlen || needle[0] == '\0') {
        *out = val_integer(0);
        return ERR_NONE;
    }
    char *p = strstr(haystack + start, needle);
    if (p)
        *out = val_integer((int)(p - haystack) + 1);
    else
        *out = val_integer(0);
    return ERR_NONE;
}

/* INSTRREV(haystack$, needle$ [, start]) - search from right */
static error_t fn_instrrev(value_t *args, int nargs, value_t *out) {
    const char *haystack, *needle;
    int hlen, nlen, start;
    if (nargs == 2) {
        if (args[0].type != VAL_STRING || args[1].type != VAL_STRING)
            return ERR_TYPE_MISMATCH;
        haystack = args[0].sval->data;
        hlen = args[0].sval->len;
        needle = args[1].sval->data;
        nlen = args[1].sval->len;
        start = hlen;
    } else if (nargs == 3) {
        if (args[0].type != VAL_STRING || args[1].type != VAL_STRING)
            return ERR_TYPE_MISMATCH;
        haystack = args[0].sval->data;
        hlen = args[0].sval->len;
        needle = args[1].sval->data;
        nlen = args[1].sval->len;
        EVAL_CHECK(val_to_integer(&args[2], &start));
    } else {
        return ERR_ILLEGAL_FUNCTION_CALL;
    }
    if (nlen == 0 || start < 1) { *out = val_integer(0); return ERR_NONE; }
    /* Search backwards from start position */
    int last = start - 1;
    if (last + nlen > hlen) last = hlen - nlen;
    for (int i = last; i >= 0; i--) {
        if (memcmp(haystack + i, needle, nlen) == 0) {
            *out = val_integer(i + 1);
            return ERR_NONE;
        }
    }
    *out = val_integer(0);
    return ERR_NONE;
}

/* UCASE$(str) */
static error_t fn_ucase(value_t *args, int nargs, value_t *out) {
    if (nargs != 1 || args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    int len = args[0].sval->len;
    char *buf = malloc(len + 1);
    if (!buf) return ERR_OUT_OF_MEMORY;
    for (int i = 0; i < len; i++)
        buf[i] = toupper((unsigned char)args[0].sval->data[i]);
    buf[len] = '\0';
    *out = val_string(buf, len);
    free(buf);
    return ERR_NONE;
}

/* LCASE$(str) */
static error_t fn_lcase(value_t *args, int nargs, value_t *out) {
    if (nargs != 1 || args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    int len = args[0].sval->len;
    char *buf = malloc(len + 1);
    if (!buf) return ERR_OUT_OF_MEMORY;
    for (int i = 0; i < len; i++)
        buf[i] = tolower((unsigned char)args[0].sval->data[i]);
    buf[len] = '\0';
    *out = val_string(buf, len);
    free(buf);
    return ERR_NONE;
}

/* LTRIM$(str) */
static error_t fn_ltrim(value_t *args, int nargs, value_t *out) {
    if (nargs != 1 || args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    const char *s = args[0].sval->data;
    int len = args[0].sval->len;
    int i = 0;
    while (i < len && s[i] == ' ') i++;
    *out = val_string(s + i, len - i);
    return ERR_NONE;
}

/* RTRIM$(str) */
static error_t fn_rtrim(value_t *args, int nargs, value_t *out) {
    if (nargs != 1 || args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    const char *s = args[0].sval->data;
    int len = args[0].sval->len;
    while (len > 0 && s[len - 1] == ' ') len--;
    *out = val_string(s, len);
    return ERR_NONE;
}

/* TRIM$(str) */
static error_t fn_trim(value_t *args, int nargs, value_t *out) {
    if (nargs != 1 || args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    const char *s = args[0].sval->data;
    int len = args[0].sval->len;
    int i = 0;
    while (i < len && s[i] == ' ') i++;
    while (len > i && s[len - 1] == ' ') len--;
    *out = val_string(s + i, len - i);
    return ERR_NONE;
}

/* SPACE$(n) - return n spaces */
static error_t fn_space(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int n;
    EVAL_CHECK(val_to_integer(&args[0], &n));
    if (n < 0) return ERR_ILLEGAL_FUNCTION_CALL;
    if (n > MAX_STRING_LEN) n = MAX_STRING_LEN;
    char *buf = malloc(n + 1);
    if (!buf) return ERR_OUT_OF_MEMORY;
    memset(buf, ' ', n);
    buf[n] = '\0';
    *out = val_string(buf, n);
    free(buf);
    return ERR_NONE;
}

/* STRING$(n, char_or_code) - repeat a character */
static error_t fn_string(value_t *args, int nargs, value_t *out) {
    if (nargs != 2) return ERR_ILLEGAL_FUNCTION_CALL;
    int n;
    EVAL_CHECK(val_to_integer(&args[0], &n));
    if (n < 0) return ERR_ILLEGAL_FUNCTION_CALL;
    if (n > MAX_STRING_LEN) n = MAX_STRING_LEN;
    char ch;
    if (args[1].type == VAL_STRING) {
        if (args[1].sval->len == 0) return ERR_ILLEGAL_FUNCTION_CALL;
        ch = args[1].sval->data[0];
    } else {
        int code;
        EVAL_CHECK(val_to_integer(&args[1], &code));
        if (code < 0 || code > 255) return ERR_ILLEGAL_FUNCTION_CALL;
        ch = (char)code;
    }
    char *buf = malloc(n + 1);
    if (!buf) return ERR_OUT_OF_MEMORY;
    memset(buf, ch, n);
    buf[n] = '\0';
    *out = val_string(buf, n);
    free(buf);
    return ERR_NONE;
}

/* MKI$(n) - pack integer into 2-byte string (little-endian) */
static error_t fn_mki(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int v; EVAL_CHECK(val_to_integer(&args[0], &v));
    char buf[2];
    buf[0] = (char)(v & 0xFF);
    buf[1] = (char)((v >> 8) & 0xFF);
    *out = val_string(buf, 2);
    return ERR_NONE;
}

/* MKL$(n) - pack long integer into 4-byte string (little-endian) */
static error_t fn_mkl(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int v; EVAL_CHECK(val_to_integer(&args[0], &v));
    char buf[4];
    buf[0] = (char)(v & 0xFF);
    buf[1] = (char)((v >> 8) & 0xFF);
    buf[2] = (char)((v >> 16) & 0xFF);
    buf[3] = (char)((v >> 24) & 0xFF);
    *out = val_string(buf, 4);
    return ERR_NONE;
}

/* MKD$(n) - pack double into 8-byte string */
static error_t fn_mkd(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    char buf[8];
    memcpy(buf, &v, 8);
    *out = val_string(buf, 8);
    return ERR_NONE;
}

/* MKS$(n) - pack single-precision float into 4-byte string (little-endian) */
static error_t fn_mks(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    float f = (float)v;
    char buf[4];
    memcpy(buf, &f, 4);
    *out = val_string(buf, 4);
    return ERR_NONE;
}

/* CVS(s$) - unpack 4-byte string to single-precision float */
static error_t fn_cvs(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    if (!args[0].sval || args[0].sval->len < 4) return ERR_ILLEGAL_FUNCTION_CALL;
    float f;
    memcpy(&f, args[0].sval->data, 4);
    *out = val_double((double)f);
    return ERR_NONE;
}

/* CVI(s$) - unpack 2-byte string to integer (little-endian) */
static error_t fn_cvi(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    if (!args[0].sval || args[0].sval->len < 2) return ERR_ILLEGAL_FUNCTION_CALL;
    const unsigned char *p = (const unsigned char *)args[0].sval->data;
    int v = (int)(short)(p[0] | (p[1] << 8));
    *out = val_integer(v);
    return ERR_NONE;
}

/* CVL(s$) - unpack 4-byte string to long integer (little-endian) */
static error_t fn_cvl(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    if (!args[0].sval || args[0].sval->len < 4) return ERR_ILLEGAL_FUNCTION_CALL;
    const unsigned char *p = (const unsigned char *)args[0].sval->data;
    int v = (int)(p[0] | (p[1] << 8) | (p[2] << 16) | (p[3] << 24));
    *out = val_integer(v);
    return ERR_NONE;
}

/* CVD(s$) - unpack 8-byte string to double */
static error_t fn_cvd(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    if (!args[0].sval || args[0].sval->len < 8) return ERR_ILLEGAL_FUNCTION_CALL;
    double v;
    memcpy(&v, args[0].sval->data, 8);
    *out = val_double(v);
    return ERR_NONE;
}

/* CURDIR$() - return current working directory */
static error_t fn_curdir(value_t *args, int nargs, value_t *out) {
    if (nargs != 0) return ERR_ILLEGAL_FUNCTION_CALL;
    char buf[256];
    if (!getcwd(buf, sizeof(buf))) {
        *out = val_string_cstr("");
        return ERR_NONE;
    }
    *out = val_string_cstr(buf);
    return ERR_NONE;
}

/* PEEK/POKE simulated memory state */
int console_width = 80; /* WIDTH setting */
/* PEEK(offset): the byte at DEF SEG's segment there (machine.c) */
static error_t fn_peek(value_t *args, int nargs, value_t *out) {
    int offset, v;
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    EVAL_CHECK(val_to_integer(&args[0], &offset));
    EVAL_CHECK(pc_peek(offset, &v));
    *out = val_integer(v);
    return ERR_NONE;
}

/* STRIG(n) - joystick trigger state (stub, returns 0) */
static error_t fn_strig(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    (void)args;
    *out = val_integer(0);
    return ERR_NONE;
}

/* STICK(n) - joystick position (stub, returns 0) */
static error_t fn_stick(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    (void)args;
    *out = val_integer(0);
    return ERR_NONE;
}

/* SEEK(n) - return current file position (1-based) */
static error_t fn_seek(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int handle;
    EVAL_CHECK(val_to_integer(&args[0], &handle));
    FILE *fp = fileio_get(handle);
    if (!fp) { *out = val_integer(0); return ERR_NONE; }
    long pos = ftell(fp);
    *out = val_integer(pos < 0 ? 1 : (int)(pos + 1));
    return ERR_NONE;
}

/* INP(port) - hardware port read (stub, returns 0) */
static error_t fn_inp(value_t *args, int nargs, value_t *out) {
    int port, v;
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    EVAL_CHECK(val_to_integer(&args[0], &port));
    EVAL_CHECK(pc_inp(port, &v));       /* (machine.c: the VGA's ports, the rest 0) */
    *out = val_integer(v);
    return ERR_NONE;
}

/* VARPTR(var) - return dummy address for variable */
static error_t fn_varptr(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    (void)args;
    *out = val_integer(0);
    return ERR_NONE;
}

/* SADD(str$) - return dummy string address */
static error_t fn_sadd(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    (void)args;
    *out = val_integer(0);
    return ERR_NONE;
}

/* LPOS(n) - the printer's column, 1-based */
static error_t fn_lpos(value_t *args, int nargs, value_t *out) {
    extern int lprint_col;
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    (void)args;
    *out = val_integer(lprint_col + 1);
    return ERR_NONE;
}

/* FRE(x) - return free memory (QBasic compat, returns constant) */
static error_t fn_fre(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    (void)args; /* argument ignored per QBasic convention */
    *out = val_integer(65536);
    return ERR_NONE;
}

/* FILEEXISTS(path$) - return -1 (true) if file exists, 0 (false) otherwise */
static error_t fn_fileexists(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    const char *path = args[0].sval ? args[0].sval->data : "";
    FILE *f = fopen(path, "r");
    if (f) {
        fclose(f);
        *out = val_integer(-1); /* true */
    } else {
        *out = val_integer(0);  /* false */
    }
    return ERR_NONE;
}

/* HEX$(n), OCT$(n): n's digits in base 16 or 8, as QuickBASIC prints
 * them -- a negative INTEGER's 16 bits (HEX$(-1) is FFFF), anything
 * else's 32 (HEX$(-1&), HEX$(-1!): FFFFFFFF) */
static error_t radix_string(value_t *args, int nargs, int shift, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int v;
    EVAL_CHECK(val_to_integer(&args[0], &v));
    unsigned int uv = (unsigned int)v;
    if (args[0].type == VAL_INTEGER) uv &= 0xFFFF;
    char tmp[16], buf[16];
    int tpos = 0, pos = 0;
    do {
        int d = uv & ((1u << shift) - 1);
        tmp[tpos++] = (char)(d < 10 ? '0' + d : 'A' + d - 10);
        uv >>= shift;
    } while (uv > 0);
    while (tpos > 0) buf[pos++] = tmp[--tpos];
    buf[pos] = '\0';
    *out = val_string_cstr(buf);
    return ERR_NONE;
}

static error_t fn_hex(value_t *args, int nargs, value_t *out) { return radix_string(args, nargs, 4, out); }
static error_t fn_oct(value_t *args, int nargs, value_t *out) { return radix_string(args, nargs, 3, out); }

/* BIN$(n) - binary string */
static error_t fn_bin(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int v;
    EVAL_CHECK(val_to_integer(&args[0], &v));
    char buf[33];
    unsigned int uv = (unsigned int)v;
    int pos = 0;
    if (uv == 0) {
        buf[pos++] = '0';
    } else {
        char tmp[33];
        int tpos = 0;
        while (uv > 0) {
            tmp[tpos++] = '0' + (uv & 1);
            uv >>= 1;
        }
        while (tpos > 0) buf[pos++] = tmp[--tpos];
    }
    buf[pos] = '\0';
    *out = val_string_cstr(buf);
    return ERR_NONE;
}

/* REPLACE$(str, find, replacement) */
static error_t fn_replace(value_t *args, int nargs, value_t *out) {
    if (nargs != 3) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING || args[1].type != VAL_STRING ||
        args[2].type != VAL_STRING)
        return ERR_TYPE_MISMATCH;
    const char *src = args[0].sval->data;
    int slen = args[0].sval->len;
    const char *find = args[1].sval->data;
    int flen = args[1].sval->len;
    const char *rep = args[2].sval->data;
    int rlen = args[2].sval->len;
    if (flen == 0) {
        *out = val_copy(&args[0]);
        return ERR_NONE;
    }
    /* Build result */
    char buf[MAX_STRING_LEN + 1];
    int pos = 0, i = 0;
    while (i <= slen - flen && pos < MAX_STRING_LEN) {
        if (memcmp(src + i, find, flen) == 0) {
            int copy = rlen;
            if (pos + copy > MAX_STRING_LEN) copy = MAX_STRING_LEN - pos;
            memcpy(buf + pos, rep, copy);
            pos += copy;
            i += flen;
        } else {
            buf[pos++] = src[i++];
        }
    }
    /* Copy remainder */
    while (i < slen && pos < MAX_STRING_LEN) buf[pos++] = src[i++];
    buf[pos] = '\0';
    *out = val_string(buf, pos);
    return ERR_NONE;
}

/* TIMER - seconds since midnight */
static error_t fn_timer(value_t *args, int nargs, value_t *out) {
    if (nargs != 0) return ERR_ILLEGAL_FUNCTION_CALL;
    struct timespec ts;
    double frac = 0.0;
    time_t now;
    if (clock_gettime(CLOCK_REALTIME, &ts) == 0) {
        now = ts.tv_sec;
        frac = (double)ts.tv_nsec / 1e9;
    } else {
        now = time(NULL);
    }
    struct tm *tm = localtime(&now);
    if (!tm) { *out = val_integer(0); return ERR_NONE; }
    *out = val_double((double)(tm->tm_hour * 3600 + tm->tm_min * 60 +
                               tm->tm_sec) + frac);
    return ERR_NONE;
}

/* DATE$ - return current date as "MM/DD/YYYY" */
static error_t fn_date(value_t *args, int nargs, value_t *out) {
    if (nargs != 0) return ERR_ILLEGAL_FUNCTION_CALL;
    time_t now = time(NULL);
    struct tm *tm = localtime(&now);
    if (!tm) { *out = val_string_cstr(""); return ERR_NONE; }
    char buf[16];
    snprintf(buf, sizeof(buf), "%02d/%02d/%04d",
             tm->tm_mon + 1, tm->tm_mday, tm->tm_year + 1900);
    *out = val_string_cstr(buf);
    return ERR_NONE;
}

/* TIME$ - return current time as "HH:MM:SS" */
static error_t fn_time_str(value_t *args, int nargs, value_t *out) {
    if (nargs != 0) return ERR_ILLEGAL_FUNCTION_CALL;
    time_t now = time(NULL);
    struct tm *tm = localtime(&now);
    if (!tm) { *out = val_string_cstr(""); return ERR_NONE; }
    char buf[16];
    snprintf(buf, sizeof(buf), "%02d:%02d:%02d",
             tm->tm_hour, tm->tm_min, tm->tm_sec);
    *out = val_string_cstr(buf);
    return ERR_NONE;
}

/* TAB(n): to column n (1-based); a column already passed is on the next
 * line, and n below 1 is 1 (QuickBASIC, GW-BASIC).  The spaces it
 * writes from the column PRINT is at, *nl when a new line comes first
 * (PRINT's own TAB writes them without a string: eval.c print_tab_spc) */
int sb_tab_spaces(int n, int *nl) {
    extern int print_col;   /* on the screen, past the width wraps */
    if (active_print_col == &print_col && console_width > 0 && n > console_width)
        n = (n - 1) % console_width + 1;
    if (n < 1) n = 1;
    n--;  /* convert 1-based column to 0-based */
    int spaces = n - *active_print_col;
    *nl = 0;
    if (spaces < 0) { *nl = 1; spaces = n; }
    if (spaces > MAX_STRING_LEN - 1) spaces = MAX_STRING_LEN - 1;
    return spaces;
}

/* SPC(n)'s spaces */
int sb_spc_spaces(int n) {
    if (n < 0) n = 0;
    if (n > MAX_STRING_LEN) n = MAX_STRING_LEN;
    return n;
}

static error_t fn_tab(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int n, nl;
    EVAL_CHECK(val_to_integer(&args[0], &n));
    int spaces = sb_tab_spaces(n, &nl);
    char *buf = malloc(spaces + 2);
    if (!buf) return ERR_OUT_OF_MEMORY;
    if (nl) buf[0] = '\n';
    memset(buf + nl, ' ', spaces);
    buf[spaces + nl] = '\0';
    *out = val_string(buf, spaces + nl);
    free(buf);
    return ERR_NONE;
}

/* LOC(n) - current byte position in file */
static error_t fn_loc(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int handle;
    EVAL_CHECK(val_to_integer(&args[0], &handle));
    FILE *fp = fileio_get(handle);
    if (!fp) { *out = val_integer(0); return ERR_NONE; }
    long pos = ftell(fp);
    *out = val_integer(pos < 0 ? 0 : (int)pos);
    return ERR_NONE;
}

/* LOF(n) - length of file in bytes */
static error_t fn_lof(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int handle;
    EVAL_CHECK(val_to_integer(&args[0], &handle));
    FILE *fp = fileio_get(handle);
    if (!fp) { *out = val_integer(0); return ERR_NONE; }
    long saved = ftell(fp);
    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    fseek(fp, saved, SEEK_SET);
    *out = val_integer(size < 0 ? 0 : (int)size);
    return ERR_NONE;
}

/* CSRLIN - current cursor row (1-based) */
static error_t fn_csrlin(value_t *args, int nargs, value_t *out) {
    (void)args;
    if (nargs != 0) return ERR_ILLEGAL_FUNCTION_CALL;
    *out = val_integer(eval_cursor_row());     /* (a full line: the next one's start) */
    return ERR_NONE;
}

/* POS(0) - current cursor column (1-based) */
static error_t fn_pos(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    (void)args; /* argument ignored, QBasic convention */
    *out = val_integer(eval_cursor_col());
    return ERR_NONE;
}

/* INPUT$(n [, #handle]) - read n characters from file or stdin */
static error_t fn_input_str(value_t *args, int nargs, value_t *out) {
    if (nargs < 1 || nargs > 2) return ERR_ILLEGAL_FUNCTION_CALL;
    int n;
    EVAL_CHECK(val_to_integer(&args[0], &n));
    if (n < 0) return ERR_ILLEGAL_FUNCTION_CALL;
    if (n > MAX_STRING_LEN) n = MAX_STRING_LEN;
    char *buf = malloc(n + 1);
    if (!buf) return ERR_OUT_OF_MEMORY;
    int count = 0;
    if (nargs == 2) {
        int handle;
        EVAL_CHECK(val_to_integer(&args[1], &handle));
        FILE *fp = fileio_get(handle);
        if (!fp) { free(buf); return ERR_BAD_FILE_NUMBER; }
        while (count < n) {
            int ch = fgetc(fp);
            if (ch == EOF) break;
            buf[count++] = (char)ch;
        }
    } else {
        while (count < n) {
            int ch = getchar();
            if (ch == EOF) break;
            buf[count++] = (char)ch;
        }
    }
    buf[count] = '\0';
    *out = val_string(buf, count);
    free(buf);
    return ERR_NONE;
}

/* EOF(n) - check end of file: returns -1 (true) or 0 (false) */
static error_t fn_eof(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int handle;
    EVAL_CHECK(val_to_integer(&args[0], &handle));
    *out = val_integer(fileio_eof(handle));
    return ERR_NONE;
}

/* FREEFILE - next available file handle */
static error_t fn_freefile(value_t *args, int nargs, value_t *out) {
    if (nargs != 0) return ERR_ILLEGAL_FUNCTION_CALL;
    *out = val_integer(fileio_freefile());
    return ERR_NONE;
}

/* SPC(n) - return n spaces */
static error_t fn_spc(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int n;
    EVAL_CHECK(val_to_integer(&args[0], &n));
    n = sb_spc_spaces(n);
    char *buf = malloc(n + 1);
    if (!buf) return ERR_OUT_OF_MEMORY;
    memset(buf, ' ', n);
    buf[n] = '\0';
    *out = val_string(buf, n);
    free(buf);
    return ERR_NONE;
}

/* ================================================================
 * System integration builtins
 * ================================================================ */

/* ENVIRON$(name$) - get environment variable */
static error_t fn_environ(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    const char *val = getenv(args[0].sval->data);
    if (val)
        *out = val_string_cstr(val);
    else
        *out = val_string_cstr("");
    return ERR_NONE;
}

/* COMMAND$([n]) - get command-line arguments */
static error_t fn_command(value_t *args, int nargs, value_t *out) {
    if (nargs > 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int argc = 0;
    char **argv = NULL;
    if (__slow32_fetch_args(&argc, &argv) != 0 || argc == 0) {
        *out = val_string_cstr("");
        return ERR_NONE;
    }
    if (nargs == 1) {
        /* COMMAND$(n) - return nth argument (1-based, 0 = program name) */
        int n;
        EVAL_CHECK(val_to_integer(&args[0], &n));
        if (n >= 0 && n < argc)
            *out = val_string_cstr(argv[n]);
        else
            *out = val_string_cstr("");
        return ERR_NONE;
    }
    /* COMMAND$() - return all args after program name as one string */
    char buf[MAX_STRING_LEN + 1];
    int pos = 0;
    for (int i = 1; i < argc && pos < MAX_STRING_LEN; i++) {
        if (i > 1 && pos < MAX_STRING_LEN) buf[pos++] = ' ';
        int len = (int)strlen(argv[i]);
        if (pos + len > MAX_STRING_LEN) len = MAX_STRING_LEN - pos;
        memcpy(buf + pos, argv[i], len);
        pos += len;
    }
    buf[pos] = '\0';
    *out = val_string(buf, pos);
    return ERR_NONE;
}

/* INKEY$ - non-blocking single character read. With a graphics
 * screen open, the tube viewer's key queue is polled first (extended
 * keys come back GW-style as CHR$(0) + scancode). */
static error_t fn_inkey(value_t *args, int nargs, value_t *out) {
    (void)args;
    if (nargs != 0) return ERR_ILLEGAL_FUNCTION_CALL;
    char ch[3];
    if (sb_gfx_active()) {
        int n = sb_gfx_inkey(ch);
        if (n > 0) {
            *out = val_string(ch, n);
            return ERR_NONE;
        }
    }
    if (sb_term_inkey(ch)) {
        *out = val_string(ch, 1);
    } else {
        *out = val_string_cstr("");
    }
    return ERR_NONE;
}

/* POINT(x, y) - pixel value at (x, y), or -1 */
/* POINT(x, y): the pixel's color at the world's point (-1 off the
 * viewport); POINT(n): the last point, n 0 and 1 the view's pixel, 2
 * and 3 the world's */
static error_t fn_point(value_t *args, int nargs, value_t *out) {
    double x, y, r;
    int ax, ay;
    if (nargs == 1) {
        EVAL_CHECK(get_num(&args[0], &x));
        if (x < 0 || x >= 4 || (!sb_gfx_active() && !sb_gfx_degraded())) return ERR_ILLEGAL_FUNCTION_CALL;
        if (sb_gfx_point_n((int)x, &r) < 0) return ERR_OVERFLOW;
        *out = x < 2 ? val_integer((int)r) : val_single(r);
        return ERR_NONE;
    }
    if (nargs != 2) return ERR_ILLEGAL_FUNCTION_CALL;
    EVAL_CHECK(get_num(&args[0], &x));
    EVAL_CHECK(get_num(&args[1], &y));
    if (!sb_gfx_active() && !sb_gfx_degraded()) return ERR_ILLEGAL_FUNCTION_CALL;  /* (text mode, as PSET) */
    if (sb_gfx_map(x, y, &ax, &ay) < 0) return ERR_OVERFLOW;
    *out = val_integer(sb_gfx_point(ax, ay));
    return ERR_NONE;
}

/* SCREEN(row, col[, attribute]): the text screen's (eval_screen_at) */
static error_t fn_screen(value_t *args, int nargs, value_t *out) {
    int r, c, a = 0;
    if (nargs < 2 || nargs > 3) return ERR_ILLEGAL_FUNCTION_CALL;
    EVAL_CHECK(val_to_integer(&args[0], &r));
    EVAL_CHECK(val_to_integer(&args[1], &c));
    if (nargs == 3) EVAL_CHECK(val_to_integer(&args[2], &a));
    return eval_screen_at(r, c, a, out);
}

/* PLAY(n): the notes waiting to be played -- none: PLAY plays nothing here */
static error_t fn_play(value_t *args, int nargs, value_t *out) {
    double n;
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    EVAL_CHECK(get_num(&args[0], &n));
    *out = val_integer(0);
    return ERR_NONE;
}

/* PMAP(c, n): a coordinate mapped between the world's (WINDOW) and the
 * view's pixels (VIEW), n 0 and 1 to the pixel's x and y (rounded as
 * CINT rounds), 2 and 3 back (a SINGLE) */
static error_t fn_pmap(value_t *args, int nargs, value_t *out) {
    double c, n;
    if (nargs != 2) return ERR_ILLEGAL_FUNCTION_CALL;
    EVAL_CHECK(get_num(&args[0], &c));
    EVAL_CHECK(get_num(&args[1], &n));
    if (n < 0 || n > 3 || (!sb_gfx_active() && !sb_gfx_degraded()))
        return ERR_ILLEGAL_FUNCTION_CALL;          /* (QBasic: in text mode too) */
    if (sb_gfx_pmap(c, (int)n, &c) < 0) return ERR_OVERFLOW;
    *out = n < 2 ? val_integer((int)c) : val_single(c);
    return ERR_NONE;
}

/* DIR$([pattern$]) - directory listing with simple wildcard matching */
static DIR *dir_handle = NULL;
static char dir_pattern[256] = "";
static char dir_path[256] = ".";

/* Simple wildcard match: * matches any chars, ? matches one char */
static int wildmatch(const char *pattern, const char *str) {
    while (*pattern) {
        if (*pattern == '*') {
            pattern++;
            if (*pattern == '\0') return 1;
            while (*str) {
                if (wildmatch(pattern, str)) return 1;
                str++;
            }
            return 0;
        } else if (*pattern == '?') {
            if (*str == '\0') return 0;
            pattern++; str++;
        } else {
            if (toupper((unsigned char)*pattern) != toupper((unsigned char)*str))
                return 0;
            pattern++; str++;
        }
    }
    return *str == '\0';
}

static error_t fn_dir(value_t *args, int nargs, value_t *out) {
    if (nargs > 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (nargs == 1) {
        /* DIR$(pattern$) - start new search */
        if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
        if (dir_handle) { closedir(dir_handle); dir_handle = NULL; }

        const char *spec = args[0].sval->data;
        /* Split into path and pattern at last / */
        const char *last_slash = NULL;
        for (const char *p = spec; *p; p++)
            if (*p == '/') last_slash = p;

        if (last_slash) {
            int plen = (int)(last_slash - spec);
            if (plen >= (int)sizeof(dir_path)) plen = sizeof(dir_path) - 1;
            memcpy(dir_path, spec, plen);
            dir_path[plen] = '\0';
            strncpy(dir_pattern, last_slash + 1, sizeof(dir_pattern) - 1);
            dir_pattern[sizeof(dir_pattern) - 1] = '\0';
        } else {
            strcpy(dir_path, ".");
            strncpy(dir_pattern, spec, sizeof(dir_pattern) - 1);
            dir_pattern[sizeof(dir_pattern) - 1] = '\0';
        }
        if (dir_pattern[0] == '\0') strcpy(dir_pattern, "*");

        dir_handle = opendir(dir_path);
        if (!dir_handle) {
            *out = val_string_cstr("");
            return ERR_NONE;
        }
    }
    /* Return next matching entry */
    if (!dir_handle) {
        *out = val_string_cstr("");
        return ERR_NONE;
    }
    struct dirent *ent;
    while ((ent = readdir(dir_handle)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
            continue;
        if (wildmatch(dir_pattern, ent->d_name)) {
            *out = val_string_cstr(ent->d_name);
            return ERR_NONE;
        }
    }
    /* End of directory */
    closedir(dir_handle);
    dir_handle = NULL;
    *out = val_string_cstr("");
    return ERR_NONE;
}

/* MIN(a, b) / MAX(a, b) */
static error_t fn_min(value_t *args, int nargs, value_t *out) {
    if (nargs < 2) return ERR_ILLEGAL_FUNCTION_CALL;
    double a, b;
    EVAL_CHECK(get_num(&args[0], &a));
    EVAL_CHECK(get_num(&args[1], &b));
    *out = val_double(a < b ? a : b);
    return ERR_NONE;
}

static error_t fn_max(value_t *args, int nargs, value_t *out) {
    if (nargs < 2) return ERR_ILLEGAL_FUNCTION_CALL;
    double a, b;
    EVAL_CHECK(get_num(&args[0], &a));
    EVAL_CHECK(get_num(&args[1], &b));
    *out = val_double(a > b ? a : b);
    return ERR_NONE;
}

/* FLOOR(x) / CEIL(x) */
static error_t fn_floor(value_t *args, int nargs, value_t *out) {
    if (nargs < 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(floor(v));
    return ERR_NONE;
}

static error_t fn_ceil(value_t *args, int nargs, value_t *out) {
    if (nargs < 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    /* ceil: smallest integer >= v */
    double f = floor(v);
    *out = val_double(v == f ? f : f + 1.0);
    return ERR_NONE;
}

/* REVERSE$(str) */
static error_t fn_reverse(value_t *args, int nargs, value_t *out) {
    if (nargs < 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    const char *src = args[0].sval ? args[0].sval->data : "";
    int len = (int)strlen(src);
    char *buf = malloc(len + 1);
    if (!buf) return ERR_OUT_OF_MEMORY;
    for (int i = 0; i < len; i++)
        buf[i] = src[len - 1 - i];
    buf[len] = '\0';
    *out = val_string_cstr(buf);
    free(buf);
    return ERR_NONE;
}

/* ================================================================
 * RANDOMIZE support (called from eval, not dispatch table)
 * ================================================================ */

/* RANDOMIZE n: the high word of n as a double, folded to 16 bits, is the
 * seed's upper two bytes (QuickBASIC's way; the low byte is kept) */
void builtin_randomize(double seed) {
    union { double d; unsigned int w[2]; } b;
    unsigned int m;
    b.d = seed;
    m = b.w[1];
    m ^= m >> 16;
    rnd_seed = ((m & 0xFFFF) << 8) | (rnd_seed & 0xFF);
}

/* a new run starts the sequence over */
void builtin_rnd_reset(void) {
    rnd_seed = 0x50000;
}

/* ================================================================
 * Dispatch table
 * ================================================================ */

typedef struct {
    const char *name;
    error_t (*fn)(value_t *args, int nargs, value_t *out);
    int min_args, max_args;     /* what fn checks when it runs; max -1: any number */
} builtin_entry_t;

static const builtin_entry_t builtins[] = {
#define BUILTIN(name, fn, min, max) { name, fn, min, max },
#include "builtins.def"
#undef BUILTIN
    { NULL, NULL, 0, 0 }
};

/* The builtin of a name, or -1: an open hash of the table's indices
 * (+1; 0 is empty), made on first use, so a name that is not a builtin
 * -- every array and variable the evaluator asks about -- costs a hash
 * and seldom a comparison */
#define BUILTIN_HASH 1024
static unsigned short builtin_hash[BUILTIN_HASH];
static int builtin_hashed;

static unsigned builtin_bucket(const char *s) {
    unsigned h = 5381;
    while (*s) h = h * 33 + (unsigned char)*s++;
    return h & (BUILTIN_HASH - 1);
}

static int builtin_index(const char *name) {
    if (!builtin_hashed) {
        for (int i = 0; builtins[i].name; i++) {
            unsigned b = builtin_bucket(builtins[i].name);
            while (builtin_hash[b]) b = (b + 1) & (BUILTIN_HASH - 1);
            builtin_hash[b] = (unsigned short)(i + 1);
        }
        builtin_hashed = 1;
    }
    for (unsigned b = builtin_bucket(name); builtin_hash[b]; b = (b + 1) & (BUILTIN_HASH - 1))
        if (strcmp(builtins[builtin_hash[b] - 1].name, name) == 0)
            return builtin_hash[b] - 1;
    return -1;
}

/* builtin_exists and builtin_arity: builtin_names.c, from the same list */

/* A numeric function's result is a double when an argument is one, a
 * single otherwise (SQR(2) prints 1.414214); CDBL and VAL say double */
error_t builtin_call(const char *name, value_t *args, int nargs, value_t *out) {
    int i = builtin_index(name);
    if (i >= 0) {
        {
            error_t err = builtins[i].fn(args, nargs, out);
            if (err == ERR_NONE && out->type == VAL_DOUBLE &&
                strcmp(name, "CDBL") != 0 && strcmp(name, "VAL") != 0) {
                int dbl = 0;
                for (int j = 0; j < nargs; j++)
                    if (args[j].type == VAL_DOUBLE) dbl = 1;
                if (!dbl) out->type = VAL_SINGLE;
            }
            return err;
        }
    }
    return ERR_ILLEGAL_FUNCTION_CALL;
}
