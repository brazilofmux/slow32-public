#ifndef SBASIC_VALUE_H
#define SBASIC_VALUE_H

#include "error.h"
#include <stdlib.h>

/* x to an integer, a half to even, as rint in the default rounding:
 * adding 2^52 leaves no fraction, the add rounding it as the hardware
 * rounds, half to even; past 2^52 every double is whole already, and a
 * NaN comes back as it went in.  Several times cheaper than libm's
 * rint (a call, its classification, ...) in the conversion every
 * subscript and integer store of a float makes.  The sign of a zero
 * result is not rint's: callers make an integer of it. */
static inline double val_rint(double x) {
    if (x >= 0 && x < 4503599627370496.0)
        return (x + 4503599627370496.0) - 4503599627370496.0;
    if (x < 0 && x > -4503599627370496.0)
        return (x - 4503599627370496.0) + 4503599627370496.0;
    return x;
}

/* Maximum string length */
#define MAX_STRING_LEN 4096

/* Value types */
typedef enum {
    VAL_INTEGER,
    VAL_DOUBLE,
    VAL_STRING,
    VAL_RECORD,
    VAL_SINGLE,     /* in dval: QuickBASIC computes in the 8087's wider
                       format and rounds to single only when it stores,
                       so a SINGLE value mid-expression is not rounded;
                       the type decides storage and how it prints */
    VAL_LONG        /* in ival, 32 bits; VAL_INTEGER is 16 (QB's % and &) */
} val_type_t;

#define VAL_IS_FLOAT(t) ((t) == VAL_SINGLE || (t) == VAL_DOUBLE)
#define VAL_IS_INT(t)   ((t) == VAL_INTEGER || (t) == VAL_LONG)
#define INT16_OK(x)     ((x) >= -32768 && (x) <= 32767)

/* Reference-counted string */
typedef struct sb_string {
    int refcount;
    int len;
    char data[1]; /* flexible array */
} sb_string_t;

/* Forward-declare sb_record_t (defined after value_t) */
typedef struct sb_record sb_record_t;

/* Value union */
typedef struct value {
    val_type_t type;
    union {
        int ival;
        double dval;
        sb_string_t *sval;
        sb_record_t *rval;
    };
} value_t;

/* Reference-counted record (TYPE instance) */
struct sb_record {
    int refcount;
    int type_idx;    /* index into type_defs[] */
    int nfields;
    value_t fields[32];
};

/* String allocation / refcounting */
sb_string_t *string_alloc(const char *s, int len);
sb_string_t *string_alloc_cstr(const char *s);
void string_ref(sb_string_t *s);
void string_unref(sb_string_t *s);
/* The one-character strings and the empty string are shared, made once
 * and never freed (a string is never changed in place); each returns a
 * new reference */
sb_string_t *string_char(int c);
sb_string_t *string_empty(void);
/* s's characters [start, start+n), in range: shared when it can be --
 * s itself when that is all of it */
sb_string_t *string_sub(sb_string_t *s, int start, int n);

/* Record allocation / refcounting */
sb_record_t *record_alloc(int type_idx, int nfields);
void record_ref(sb_record_t *r);
void record_unref(sb_record_t *r);

/* Value constructors */
value_t val_integer(int v);             /* an INTEGER, or a LONG past 16 bits */
value_t val_long(int v);
value_t val_double(double v);
value_t val_single(double v);           /* not rounded: an intermediate */
double single_round(double v);          /* to the nearest single */
value_t val_string(const char *s, int len);
value_t val_string_cstr(const char *s);
value_t val_default(val_type_t type);

/* Value destruction (unrefs strings) */
void val_clear(value_t *v);
/* the same, the call made only for what holds a reference */
#define VAL_CLEAR(v) do { if ((v)->type == VAL_STRING) { sb_string_t *s_ = (v)->sval; \
                              if (s_ && --s_->refcount <= 0) free(s_); (v)->type = VAL_INTEGER; } \
                          else if ((v)->type == VAL_RECORD) val_clear(v); \
                          else (v)->type = VAL_INTEGER; } while (0)

/* Copy a value (refs strings); inline, being everywhere a variable is
 * read */
static inline value_t val_copy(const value_t *v) {
    value_t r = *v;
    if (r.type == VAL_STRING) { if (r.sval) r.sval->refcount++; }
    else if (r.type == VAL_RECORD) record_ref(r.rval);
    return r;
}

/* Assign src to dst, clearing old dst */
void val_assign(value_t *dst, const value_t *src);

/* v made the type of a variable of type t, for storing in it: to an
 * integer by rounding half to even (Overflow out of range), to a single
 * by rounding to one; a string to or from a number is a Type mismatch */
error_t val_coerce(value_t *v, val_type_t t);

/* Overflow when a single or double is out of its range (or not a
 * number): QBasic checks when a value becomes one, to print or store */
error_t val_check_range(const value_t *v);

/* A number as QuickBASIC writes it, with no space around it: -.5,
 * .3333333, 1E+07, 1.234567890123457D+16 -- 7 significant digits for an
 * integer or single, 16 for a double, fixed point when that fits */
void val_format_number(const value_t *v, char *buf, int bufsize);

/* Type coercion */
error_t val_to_integer(const value_t *v, int *out);
/* val_to_integer of a float */
static inline error_t val_int_of(double d, int *out) {
    if (!(d < 2147483647.5 && d >= -2147483648.5)) return ERR_OVERFLOW;
    int i = (int)d;
    *out = (double)i == d ? i : (int)val_rint(d);
    return ERR_NONE;
}
error_t val_to_double(const value_t *v, double *out);
error_t val_to_string(const value_t *v, char *buf, int bufsize);

/* Get the C string pointer from a string value (or "" for non-string) */
const char *val_cstr(const value_t *v);

/* Arithmetic on values */
error_t val_add(const value_t *a, const value_t *b, value_t *result);
error_t val_sub(const value_t *a, const value_t *b, value_t *result);
error_t val_mul(const value_t *a, const value_t *b, value_t *result);
error_t val_div(const value_t *a, const value_t *b, value_t *result);
error_t val_idiv(const value_t *a, const value_t *b, value_t *result);
error_t val_mod(const value_t *a, const value_t *b, value_t *result);
error_t val_pow(const value_t *a, const value_t *b, value_t *result);
error_t val_neg(const value_t *a, value_t *result);

/* Comparison: sets *out to -1, 0, or 1 */
error_t val_compare(const value_t *a, const value_t *b, int *out);

/* Truthiness: 0 and "" are false, everything else true */
int val_is_true(const value_t *v);

/* Determine the type suffix implies */
val_type_t type_from_suffix(char suffix);

#endif
