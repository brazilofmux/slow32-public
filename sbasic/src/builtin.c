#include "builtin.h"
#include "eval.h"
#include "fileio.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>

/* Simple LCG random number generator */
static unsigned int rng_state = 12345;
static double last_rnd_val = 0.0;

static double sb_rnd(void) {
    rng_state = rng_state * 1103515245 + 12345;
    last_rnd_val = (double)(rng_state & 0x7FFFFFFF) / 2147483648.0;
    return last_rnd_val;
}

/* ================================================================
 * Helper: extract numeric argument
 * ================================================================ */
static error_t get_num(value_t *v, double *out) {
    if (v->type == VAL_STRING) return ERR_TYPE_MISMATCH;
    *out = (v->type == VAL_INTEGER) ? (double)v->ival : v->dval;
    return ERR_NONE;
}

/* ================================================================
 * Math built-in functions
 * ================================================================ */

static error_t fn_abs(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type == VAL_STRING) return ERR_TYPE_MISMATCH;
    if (args[0].type == VAL_INTEGER) {
        int v = args[0].ival;
        *out = val_integer(v < 0 ? -v : v);
    } else {
        double v = args[0].dval;
        *out = val_double(v < 0 ? -v : v);
    }
    return ERR_NONE;
}

static error_t fn_int(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_integer((int)floor(v));
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
    *out = val_integer((int)v); /* truncate toward zero */
    return ERR_NONE;
}

static error_t fn_cint(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    /* Round to nearest integer (banker's rounding not needed for BASIC) */
    *out = val_integer((int)(v + (v >= 0.0 ? 0.5 : -0.5)));
    return ERR_NONE;
}

static error_t fn_cdbl(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    double v; EVAL_CHECK(get_num(&args[0], &v));
    *out = val_double(v);
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

static error_t fn_rnd(value_t *args, int nargs, value_t *out) {
    if (nargs > 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (nargs == 1) {
        int seed;
        EVAL_CHECK(val_to_integer(&args[0], &seed));
        if (seed < 0) {
            rng_state = (unsigned int)(-seed);
            *out = val_double(sb_rnd());
            return ERR_NONE;
        } else if (seed == 0) {
            *out = val_double(last_rnd_val);
            return ERR_NONE;
        }
    }
    *out = val_double(sb_rnd());
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

static error_t fn_chr(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int code;
    EVAL_CHECK(val_to_integer(&args[0], &code));
    if (code < 0 || code > 255) return ERR_ILLEGAL_FUNCTION_CALL;
    char buf[2] = { (char)code, '\0' };
    *out = val_string(buf, 1);
    return ERR_NONE;
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
    EVAL_CHECK(val_to_string(&args[0], buf, sizeof(buf)));
    *out = val_string_cstr(buf);
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

static error_t fn_left(value_t *args, int nargs, value_t *out) {
    if (nargs != 2) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    int n;
    EVAL_CHECK(val_to_integer(&args[1], &n));
    if (n < 0) return ERR_ILLEGAL_FUNCTION_CALL;
    int len = args[0].sval->len;
    if (n > len) n = len;
    *out = val_string(args[0].sval->data, n);
    return ERR_NONE;
}

static error_t fn_right(value_t *args, int nargs, value_t *out) {
    if (nargs != 2) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    int n;
    EVAL_CHECK(val_to_integer(&args[1], &n));
    if (n < 0) return ERR_ILLEGAL_FUNCTION_CALL;
    int len = args[0].sval->len;
    if (n > len) n = len;
    *out = val_string(args[0].sval->data + len - n, n);
    return ERR_NONE;
}

static error_t fn_mid(value_t *args, int nargs, value_t *out) {
    if (nargs < 2 || nargs > 3) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type != VAL_STRING) return ERR_TYPE_MISMATCH;
    int start;
    EVAL_CHECK(val_to_integer(&args[1], &start));
    start--;
    if (start < 0) start = 0;
    int len = args[0].sval->len;
    int count = len - start;
    if (nargs == 3) {
        EVAL_CHECK(val_to_integer(&args[2], &count));
        if (count < 0) return ERR_ILLEGAL_FUNCTION_CALL;
    }
    if (start >= len) {
        *out = val_string_cstr("");
        return ERR_NONE;
    }
    if (start + count > len) count = len - start;
    *out = val_string(args[0].sval->data + start, count);
    return ERR_NONE;
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

/* HEX$(n) - hexadecimal string */
static error_t fn_hex(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int v;
    EVAL_CHECK(val_to_integer(&args[0], &v));
    char buf[16];
    /* Print as unsigned hex */
    unsigned int uv = (unsigned int)v;
    int pos = 0;
    if (uv == 0) {
        buf[pos++] = '0';
    } else {
        char tmp[16];
        int tpos = 0;
        while (uv > 0) {
            int d = uv & 0xF;
            tmp[tpos++] = d < 10 ? '0' + d : 'A' + d - 10;
            uv >>= 4;
        }
        while (tpos > 0) buf[pos++] = tmp[--tpos];
    }
    buf[pos] = '\0';
    *out = val_string_cstr(buf);
    return ERR_NONE;
}

/* OCT$(n) - octal string */
static error_t fn_oct(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int v;
    EVAL_CHECK(val_to_integer(&args[0], &v));
    char buf[16];
    unsigned int uv = (unsigned int)v;
    int pos = 0;
    if (uv == 0) {
        buf[pos++] = '0';
    } else {
        char tmp[16];
        int tpos = 0;
        while (uv > 0) {
            tmp[tpos++] = '0' + (uv & 7);
            uv >>= 3;
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

/* TIMER - seconds since midnight (integer for simplicity on SLOW-32) */
static error_t fn_timer(value_t *args, int nargs, value_t *out) {
    if (nargs != 0) return ERR_ILLEGAL_FUNCTION_CALL;
    /* No clock on SLOW-32, return 0 */
    *out = val_integer(0);
    return ERR_NONE;
}

/* TAB(n) - return spaces to reach absolute column n (1-based) */
static error_t fn_tab(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    int n;
    EVAL_CHECK(val_to_integer(&args[0], &n));
    n--;  /* convert 1-based column to 0-based */
    int spaces = n - *active_print_col;
    if (spaces < 0) spaces = 0;
    if (spaces > MAX_STRING_LEN) spaces = MAX_STRING_LEN;
    char *buf = malloc(spaces + 1);
    if (!buf) return ERR_OUT_OF_MEMORY;
    memset(buf, ' ', spaces);
    buf[spaces] = '\0';
    *out = val_string(buf, spaces);
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
    if (n < 0) n = 0;
    if (n > MAX_STRING_LEN) n = MAX_STRING_LEN;
    char *buf = malloc(n + 1);
    if (!buf) return ERR_OUT_OF_MEMORY;
    memset(buf, ' ', n);
    buf[n] = '\0';
    *out = val_string(buf, n);
    free(buf);
    return ERR_NONE;
}

/* ================================================================
 * RANDOMIZE support (called from eval, not dispatch table)
 * ================================================================ */

void builtin_randomize(int seed) {
    rng_state = (unsigned int)seed;
}

/* ================================================================
 * Dispatch table
 * ================================================================ */

typedef struct {
    const char *name;
    error_t (*fn)(value_t *args, int nargs, value_t *out);
} builtin_entry_t;

static const builtin_entry_t builtins[] = {
    /* Math */
    { "ABS",      fn_abs },
    { "INT",      fn_int },
    { "SGN",      fn_sgn },
    { "SQR",      fn_sqr },
    { "FIX",      fn_fix },
    { "CINT",     fn_cint },
    { "CDBL",     fn_cdbl },
    { "SIN",      fn_sin },
    { "COS",      fn_cos },
    { "TAN",      fn_tan },
    { "ATN",      fn_atn },
    { "ATN2",     fn_atn2 },
    { "EXP",      fn_exp },
    { "LOG",      fn_log },
    { "LOG10",    fn_log10 },
    { "RND",      fn_rnd },
    { "TIMER",    fn_timer },
    { "TAB",      fn_tab },
    { "SPC",      fn_spc },
    /* String */
    { "LEN",      fn_len },
    { "CHR$",     fn_chr },
    { "ASC",      fn_asc },
    { "STR$",     fn_str },
    { "VAL",      fn_val },
    { "LEFT$",    fn_left },
    { "RIGHT$",   fn_right },
    { "MID$",     fn_mid },
    { "INSTR",    fn_instr },
    { "UCASE$",   fn_ucase },
    { "LCASE$",   fn_lcase },
    { "LTRIM$",   fn_ltrim },
    { "RTRIM$",   fn_rtrim },
    { "TRIM$",    fn_trim },
    { "SPACE$",   fn_space },
    { "STRING$",  fn_string },
    { "HEX$",     fn_hex },
    { "OCT$",     fn_oct },
    { "REPLACE$", fn_replace },
    /* File I/O */
    { "EOF",      fn_eof },
    { "FREEFILE", fn_freefile },
    { NULL, NULL }
};

int builtin_exists(const char *name) {
    for (int i = 0; builtins[i].name; i++) {
        if (strcmp(builtins[i].name, name) == 0)
            return 1;
    }
    return 0;
}

error_t builtin_call(const char *name, value_t *args, int nargs, value_t *out) {
    for (int i = 0; builtins[i].name; i++) {
        if (strcmp(builtins[i].name, name) == 0)
            return builtins[i].fn(args, nargs, out);
    }
    return ERR_ILLEGAL_FUNCTION_CALL;
}
