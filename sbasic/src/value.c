#include "value.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

/* --- String refcounting --- */

sb_string_t *string_alloc(const char *s, int len) {
    sb_string_t *str = malloc(sizeof(sb_string_t) + len);
    if (!str) return NULL;
    str->refcount = 1;
    str->len = len;
    if (s)
        memcpy(str->data, s, len);
    str->data[len] = '\0';
    return str;
}

sb_string_t *string_alloc_cstr(const char *s) {
    return string_alloc(s, s ? (int)strlen(s) : 0);
}

void string_ref(sb_string_t *s) {
    if (s) s->refcount++;
}

void string_unref(sb_string_t *s) {
    if (s) {
        s->refcount--;
        if (s->refcount <= 0)
            free(s);
    }
}

/* --- Record refcounting --- */

sb_record_t *record_alloc(int type_idx, int nfields) {
    sb_record_t *r = malloc(sizeof(sb_record_t));
    if (!r) return NULL;
    r->refcount = 1;
    r->type_idx = type_idx;
    r->nfields = nfields;
    for (int i = 0; i < nfields; i++) {
        r->fields[i].type = VAL_INTEGER;
        r->fields[i].ival = 0;
    }
    return r;
}

void record_ref(sb_record_t *r) {
    if (r) r->refcount++;
}

void record_unref(sb_record_t *r) {
    if (r) {
        r->refcount--;
        if (r->refcount <= 0) {
            for (int i = 0; i < r->nfields; i++)
                val_clear(&r->fields[i]);
            free(r);
        }
    }
}

/* --- Value constructors --- */

value_t val_integer(int v) {
    value_t r;
    r.type = INT16_OK(v) ? VAL_INTEGER : VAL_LONG;
    r.ival = v;
    return r;
}

value_t val_long(int v) {
    value_t r;
    r.type = VAL_LONG;
    r.ival = v;
    return r;
}

value_t val_double(double v) {
    value_t r;
    r.type = VAL_DOUBLE;
    r.dval = v;
    return r;
}

value_t val_single(double v) {
    value_t r;
    r.type = VAL_SINGLE;
    r.dval = v;
    return r;
}

double single_round(double v) {
    return (double)(float)v;
}

value_t val_string(const char *s, int len) {
    value_t r;
    r.type = VAL_STRING;
    r.sval = string_alloc(s, len);
    return r;
}

value_t val_string_cstr(const char *s) {
    value_t r;
    r.type = VAL_STRING;
    r.sval = string_alloc_cstr(s ? s : "");
    return r;
}

value_t val_default(val_type_t type) {
    switch (type) {
        case VAL_INTEGER: return val_integer(0);
        case VAL_DOUBLE:  return val_double(0.0);
        case VAL_SINGLE:  return val_single(0.0);
        case VAL_LONG:    return val_long(0);
        case VAL_STRING:  return val_string_cstr("");
        case VAL_RECORD:  return val_integer(0); /* records created via record_alloc */
    }
    return val_integer(0);
}

/* --- Value lifecycle --- */

void val_clear(value_t *v) {
    if (v->type == VAL_STRING)
        string_unref(v->sval);
    else if (v->type == VAL_RECORD)
        record_unref(v->rval);
    v->type = VAL_INTEGER;
    v->ival = 0;
}

value_t val_copy(const value_t *v) {
    value_t r = *v;
    if (r.type == VAL_STRING)
        string_ref(r.sval);
    else if (r.type == VAL_RECORD)
        record_ref(r.rval);
    return r;
}

void val_assign(value_t *dst, const value_t *src) {
    if (src->type == VAL_STRING)
        string_ref(src->sval);
    else if (src->type == VAL_RECORD)
        record_ref(src->rval);
    if (dst->type == VAL_STRING)
        string_unref(dst->sval);
    else if (dst->type == VAL_RECORD)
        record_unref(dst->rval);
    *dst = *src;
}

/* --- Type coercion --- */

static double val_as_double(const value_t *v) {
    switch (v->type) {
        case VAL_INTEGER:
        case VAL_LONG:    return (double)v->ival;
        case VAL_DOUBLE:
        case VAL_SINGLE:  return v->dval;
        case VAL_STRING:  return atof(v->sval->data);
        case VAL_RECORD:  return 0.0;
    }
    return 0.0;
}

/* QuickBASIC's conversion to an integer: rounded, a half to even */
static int round_even(double d) {
    int i = (int)d;             /* in range: val_to_integer checked */
    if ((double)i == d) return i;       /* whole already: no rint */
    return (int)rint(d);
}

static int val_as_int(const value_t *v) {
    switch (v->type) {
        case VAL_INTEGER:
        case VAL_LONG:    return v->ival;
        case VAL_DOUBLE:
        case VAL_SINGLE:  return round_even(v->dval);
        case VAL_STRING:  return atoi(v->sval->data);
        case VAL_RECORD:  return 0;
    }
    return 0;
}

error_t val_to_integer(const value_t *v, int *out) {
    if (v->type == VAL_STRING || v->type == VAL_RECORD)
        return ERR_TYPE_MISMATCH;
    if (VAL_IS_FLOAT(v->type)) {
        double d = v->dval;
        if (!(d < 2147483647.5 && d >= -2147483648.5))
            return ERR_OVERFLOW;
    }
    *out = val_as_int(v);
    return ERR_NONE;
}

error_t val_to_double(const value_t *v, double *out) {
    if (v->type == VAL_STRING || v->type == VAL_RECORD)
        return ERR_TYPE_MISMATCH;
    *out = val_as_double(v);
    return ERR_NONE;
}

error_t val_to_string(const value_t *v, char *buf, int bufsize) {
    switch (v->type) {
        case VAL_STRING:
            if (v->sval->len >= bufsize) return ERR_STRING_TOO_LONG;
            memcpy(buf, v->sval->data, v->sval->len + 1);
            break;
        case VAL_INTEGER:
        case VAL_LONG:
            snprintf(buf, bufsize, "%d", v->ival);
            break;
        case VAL_DOUBLE:
        case VAL_SINGLE:
            val_format_number(v, buf, bufsize);
            break;
        case VAL_RECORD:
            return ERR_TYPE_MISMATCH;
    }
    return ERR_NONE;
}

error_t val_coerce(value_t *v, val_type_t t) {
    if (v->type == t && t != VAL_SINGLE) return ERR_NONE;
    if (t == VAL_STRING || v->type == VAL_STRING)
        return (t == VAL_STRING && v->type == VAL_STRING) ? ERR_NONE : ERR_TYPE_MISMATCH;
    if (t == VAL_RECORD || v->type == VAL_RECORD)
        return t == v->type ? ERR_NONE : ERR_TYPE_MISMATCH;
    if (t == VAL_INTEGER || t == VAL_LONG) {
        int i;
        error_t err = val_to_integer(v, &i);
        if (err != ERR_NONE) return err;
        if (t == VAL_INTEGER && !INT16_OK(i)) return ERR_OVERFLOW;
        v->type = t;
        v->ival = i;
        return ERR_NONE;
    }
    double d = val_as_double(v);
    if (t == VAL_SINGLE) {
        if (!(fabs(d) <= 3.4028234663852886e38))
            return ERR_OVERFLOW;
        *v = val_single(single_round(d));
    } else {
        if (!(fabs(d) <= 1.7976931348623157e308))
            return ERR_OVERFLOW;
        *v = val_double(d);
    }
    return ERR_NONE;
}

error_t val_check_range(const value_t *v) {
    if (v->type == VAL_SINGLE && !(fabs(v->dval) <= 3.4028234663852886e38))
        return ERR_OVERFLOW;
    if (v->type == VAL_DOUBLE && !(fabs(v->dval) <= 1.7976931348623157e308))
        return ERR_OVERFLOW;
    return ERR_NONE;
}

void val_format_number(const value_t *v, char *buf, int bufsize) {
    char digits[40], out[64];
    int prec, n, e, i, o = 0;
    double d;
    if (VAL_IS_INT(v->type)) {
        snprintf(buf, bufsize, "%d", v->ival);
        return;
    }
    d = v->dval;
    prec = v->type == VAL_DOUBLE ? 16 : 7;
    if (d == 0.0 || d != d) {           /* zero (never -0) */
        snprintf(buf, bufsize, "0");
        return;
    }
    if (d < 0) { out[o++] = '-'; d = -d; }
    /* the significant digits, rounded to prec: d = 0.DIGITS x 10^e */
    {
        char tmp[48];
        snprintf(tmp, sizeof(tmp), "%.*e", prec - 1, d);
        n = 0;
        for (i = 0; tmp[i] && tmp[i] != 'e'; i++)
            if (tmp[i] >= '0' && tmp[i] <= '9') digits[n++] = tmp[i];
        e = atoi(tmp + i + 1) + 1;
        while (n > 1 && digits[n - 1] == '0') n--;
        digits[n] = '\0';
    }
    if (e <= prec && n - e <= prec) {
        /* fixed: no leading 0 before the point */
        if (e <= 0) {
            out[o++] = '.';
            for (i = 0; i < -e; i++) out[o++] = '0';
            for (i = 0; i < n; i++) out[o++] = digits[i];
        } else {
            for (i = 0; i < e; i++) out[o++] = i < n ? digits[i] : '0';
            if (n > e) {
                out[o++] = '.';
                for (i = e; i < n; i++) out[o++] = digits[i];
            }
        }
        out[o] = '\0';
    } else {
        out[o++] = digits[0];
        if (n > 1) {
            out[o++] = '.';
            for (i = 1; i < n; i++) out[o++] = digits[i];
        }
        snprintf(out + o, sizeof(out) - o, "%c%c%02d",
                 v->type == VAL_DOUBLE ? 'D' : 'E', e - 1 < 0 ? '-' : '+',
                 e - 1 < 0 ? 1 - e : e - 1);
    }
    snprintf(buf, bufsize, "%s", out);
}

const char *val_cstr(const value_t *v) {
    if (v->type == VAL_STRING && v->sval)
        return v->sval->data;
    return "";
}

/* --- Arithmetic --- */

#define INT_RANGE(x) ((x) >= -2147483648LL && (x) <= 2147483647LL)

/* Promote two numeric values to a common type for arithmetic: 1 if
 * either is a single or a double, 0 if both are integers */
static int promote(const value_t *a, const value_t *b) {
    return VAL_IS_FLOAT(a->type) || VAL_IS_FLOAT(b->type);
}

/* an integer result, typed as the wider of the operands (a LONG if
 * either is one), Overflow past that type's range -- QuickBASIC's
 * 32767 + 1 is an Overflow, 32767& + 1 is 32768 */
static error_t int_result(const value_t *a, const value_t *b, long long r, value_t *out) {
    int lng = a->type == VAL_LONG || (b && b->type == VAL_LONG);
    if (lng ? !INT_RANGE(r) : !INT16_OK(r))
        return ERR_OVERFLOW;
    out->type = lng ? VAL_LONG : VAL_INTEGER;
    out->ival = (int)r;
    return ERR_NONE;
}

/* the integer an operand of \ or MOD (or a logical operator) becomes:
 * its own type, or for a float an INTEGER when it fits, else a LONG */
static const value_t *int_kind(const value_t *v, int i) {
    static const value_t as_int = { VAL_INTEGER, { 0 } }, as_long = { VAL_LONG, { 0 } };
    if (VAL_IS_INT(v->type)) return v;
    return INT16_OK(i) ? &as_int : &as_long;
}

/* a floating result, typed as the wider of the operands (a single unless
 * a double is among them: integer / integer is a single) */
static value_t val_float(const value_t *a, const value_t *b, double d) {
    if (a->type == VAL_DOUBLE || (b && b->type == VAL_DOUBLE))
        return val_double(d);
    return val_single(d);
}

error_t val_add(const value_t *a, const value_t *b, value_t *result) {
    if (a->type == VAL_RECORD || b->type == VAL_RECORD)
        return ERR_TYPE_MISMATCH;
    /* String concatenation */
    if (a->type == VAL_STRING && b->type == VAL_STRING) {
        int len = a->sval->len + b->sval->len;
        if (len > MAX_STRING_LEN) return ERR_STRING_TOO_LONG;
        sb_string_t *s = string_alloc(NULL, len);
        if (!s) return ERR_OUT_OF_MEMORY;
        memcpy(s->data, a->sval->data, a->sval->len);
        memcpy(s->data + a->sval->len, b->sval->data, b->sval->len);
        s->data[len] = '\0';
        result->type = VAL_STRING;
        result->sval = s;
        return ERR_NONE;
    }
    if (a->type == VAL_STRING || b->type == VAL_STRING)
        return ERR_TYPE_MISMATCH;
    if (promote(a, b)) {
        *result = val_float(a, b, val_as_double(a) + val_as_double(b));
    } else {
        long long r = (long long)a->ival + (long long)b->ival;
        return int_result(a, b, r, result);
    }
    return ERR_NONE;
}

error_t val_sub(const value_t *a, const value_t *b, value_t *result) {
    if (a->type == VAL_STRING || b->type == VAL_STRING ||
        a->type == VAL_RECORD || b->type == VAL_RECORD)
        return ERR_TYPE_MISMATCH;
    if (promote(a, b)) {
        *result = val_float(a, b, val_as_double(a) - val_as_double(b));
    } else {
        long long r = (long long)a->ival - (long long)b->ival;
        return int_result(a, b, r, result);
    }
    return ERR_NONE;
}

error_t val_mul(const value_t *a, const value_t *b, value_t *result) {
    if (a->type == VAL_STRING || b->type == VAL_STRING ||
        a->type == VAL_RECORD || b->type == VAL_RECORD)
        return ERR_TYPE_MISMATCH;
    if (promote(a, b)) {
        *result = val_float(a, b, val_as_double(a) * val_as_double(b));
    } else {
        long long r = (long long)a->ival * (long long)b->ival;
        return int_result(a, b, r, result);
    }
    return ERR_NONE;
}

error_t val_div(const value_t *a, const value_t *b, value_t *result) {
    if (a->type == VAL_STRING || b->type == VAL_STRING ||
        a->type == VAL_RECORD || b->type == VAL_RECORD)
        return ERR_TYPE_MISMATCH;
    double db = val_as_double(b);
    if (db == 0.0) return ERR_DIVISION_BY_ZERO;
    *result = val_float(a, b, val_as_double(a) / db);
    return ERR_NONE;
}

error_t val_idiv(const value_t *a, const value_t *b, value_t *result) {
    if (a->type == VAL_STRING || b->type == VAL_STRING ||
        a->type == VAL_RECORD || b->type == VAL_RECORD)
        return ERR_TYPE_MISMATCH;
    error_t err;
    int ia, ib;
    if ((err = val_to_integer(a, &ia)) != ERR_NONE) return err;
    if ((err = val_to_integer(b, &ib)) != ERR_NONE) return err;
    if (ib == 0) return ERR_DIVISION_BY_ZERO;
    /* 32-bit division but where it overflows (INT_MIN \ -1) */
    return int_result(int_kind(a, ia), int_kind(b, ib),
                      ib == -1 ? -(long long)ia : (long long)(ia / ib), result);
}

error_t val_mod(const value_t *a, const value_t *b, value_t *result) {
    if (a->type == VAL_STRING || b->type == VAL_STRING ||
        a->type == VAL_RECORD || b->type == VAL_RECORD)
        return ERR_TYPE_MISMATCH;
    error_t err;
    int ia, ib;
    if ((err = val_to_integer(a, &ia)) != ERR_NONE) return err;
    if ((err = val_to_integer(b, &ib)) != ERR_NONE) return err;
    if (ib == 0) return ERR_DIVISION_BY_ZERO;
    return int_result(int_kind(a, ia), int_kind(b, ib),
                      ib == -1 ? 0 : (long long)(ia % ib), result);
}

error_t val_pow(const value_t *a, const value_t *b, value_t *result) {
    if (a->type == VAL_STRING || b->type == VAL_STRING ||
        a->type == VAL_RECORD || b->type == VAL_RECORD)
        return ERR_TYPE_MISMATCH;
    {
        /* 0 to a negative power, and a negative number to a fractional
         * one, are Illegal function calls (QBasic) */
        double x = val_as_double(a), y = val_as_double(b);
        if ((x == 0.0 && y < 0.0) || (x < 0.0 && y != floor(y)))
            return ERR_ILLEGAL_FUNCTION_CALL;
    }
    /* ^ is always a float (QBasic: 2 ^ 15 is 32768, no Overflow) */
    *result = val_float(a, b, pow(val_as_double(a), val_as_double(b)));
    return ERR_NONE;
}

error_t val_neg(const value_t *a, value_t *result) {
    if (a->type == VAL_STRING || a->type == VAL_RECORD)
        return ERR_TYPE_MISMATCH;
    if (VAL_IS_INT(a->type)) {
        return int_result(a, NULL, -(long long)a->ival, result);
    } else
        *result = val_float(a, NULL, -a->dval);
    return ERR_NONE;
}

/* --- Comparison --- */

error_t val_compare(const value_t *a, const value_t *b, int *out) {
    if (a->type == VAL_RECORD || b->type == VAL_RECORD)
        return ERR_TYPE_MISMATCH;
    if (a->type == VAL_STRING && b->type == VAL_STRING) {
        *out = strcmp(a->sval->data, b->sval->data);
        if (*out < 0) *out = -1;
        if (*out > 0) *out = 1;
        return ERR_NONE;
    }
    if (a->type == VAL_STRING || b->type == VAL_STRING)
        return ERR_TYPE_MISMATCH;
    double da = val_as_double(a);
    double db = val_as_double(b);
    if (da < db) *out = -1;
    else if (da > db) *out = 1;
    else *out = 0;
    return ERR_NONE;
}

int val_is_true(const value_t *v) {
    switch (v->type) {
        case VAL_INTEGER: return v->ival != 0;
        case VAL_DOUBLE:
        case VAL_SINGLE:  return v->dval != 0.0;
        case VAL_LONG:    return v->ival != 0;
        case VAL_STRING:  return v->sval && v->sval->len > 0;
        case VAL_RECORD:  return v->rval != NULL;
    }
    return 0;
}

val_type_t type_from_suffix(char suffix) {
    switch (suffix) {
        case '%': return VAL_INTEGER;
        case '&': return VAL_LONG;
        case '#': return VAL_DOUBLE;
        case '$': return VAL_STRING;
        default:  return VAL_SINGLE;
    }
}
