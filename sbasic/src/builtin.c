#include "builtin.h"
#include <string.h>
#include <stdlib.h>

/* floor/sqrt/pow may not be available as double in SLOW-32 runtime.
   Provide simple implementations. */
static double sb_floor(double x) {
    int i = (int)x;
    if (x < 0.0 && (double)i != x) return (double)(i - 1);
    return (double)i;
}

static double sb_sqrt(double x) {
    if (x <= 0.0) return 0.0;
    double guess = x;
    for (int i = 0; i < 30; i++) {
        guess = (guess + x / guess) * 0.5;
    }
    return guess;
}

static double sb_pow(double base, double exp) {
    if (exp == 0.0) return 1.0;
    if (base == 0.0) return 0.0;
    /* Integer exponent fast path */
    if (exp == (double)(int)exp && exp > 0 && exp < 100) {
        int n = (int)exp;
        double r = 1.0;
        double b = base;
        while (n > 0) {
            if (n & 1) r *= b;
            b *= b;
            n >>= 1;
        }
        return r;
    }
    /* For negative integer exponents */
    if (exp == (double)(int)exp && exp < 0 && exp > -100) {
        return 1.0 / sb_pow(base, -exp);
    }
    /* General case: use exp(exp * ln(base)) via software approximation
       For now, just return 0 for non-integer exponents (will be fixed in Stage 4) */
    return 0.0;
}

/* --- Math functions --- */

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
    if (args[0].type == VAL_STRING) return ERR_TYPE_MISMATCH;
    double v;
    if (args[0].type == VAL_INTEGER)
        v = (double)args[0].ival;
    else
        v = args[0].dval;
    *out = val_integer((int)sb_floor(v));
    return ERR_NONE;
}

static error_t fn_sgn(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type == VAL_STRING) return ERR_TYPE_MISMATCH;
    double v;
    if (args[0].type == VAL_INTEGER)
        v = (double)args[0].ival;
    else
        v = args[0].dval;
    if (v > 0) *out = val_integer(1);
    else if (v < 0) *out = val_integer(-1);
    else *out = val_integer(0);
    return ERR_NONE;
}

static error_t fn_sqr(value_t *args, int nargs, value_t *out) {
    if (nargs != 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (args[0].type == VAL_STRING) return ERR_TYPE_MISMATCH;
    double v;
    if (args[0].type == VAL_INTEGER)
        v = (double)args[0].ival;
    else
        v = args[0].dval;
    if (v < 0) return ERR_ILLEGAL_FUNCTION_CALL;
    *out = val_double(sb_sqrt(v));
    return ERR_NONE;
}

/* --- String functions --- */

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
    /* Try integer first */
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
    start--; /* BASIC is 1-based */
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

/* --- Dispatch table --- */

typedef struct {
    const char *name;
    error_t (*fn)(value_t *args, int nargs, value_t *out);
} builtin_entry_t;

static const builtin_entry_t builtins[] = {
    { "ABS",    fn_abs },
    { "INT",    fn_int },
    { "SGN",    fn_sgn },
    { "SQR",    fn_sqr },
    { "LEN",    fn_len },
    { "CHR$",   fn_chr },
    { "ASC",    fn_asc },
    { "STR$",   fn_str },
    { "VAL",    fn_val },
    { "LEFT$",  fn_left },
    { "RIGHT$", fn_right },
    { "MID$",   fn_mid },
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
