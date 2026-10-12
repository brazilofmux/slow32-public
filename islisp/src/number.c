/* number.c -- integers of any size and floats (§19).
 *
 * A fixnum carries 31 bits; past that a bignum: sign and a little-endian
 * magnitude of 32-bit limbs, always normalised (no leading zero limbs,
 * and a magnitude that fits a fixnum is a fixnum).  Schoolbook
 * multiplication and Knuth's algorithm D for division; the numbers the
 * verification suite reaches are hundreds of digits, not thousands, so
 * nothing cleverer is warranted yet.  Floats are IEEE doubles, which
 * SLOW-32 computes in hardware.
 */
#include "isl.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

static val mk_bignum(int sign, int n) {
    bignum_t *b = (bignum_t *)heap_alloc(T_BIGNUM, sizeof(bignum_t) + (size_t)(n > 0 ? n - 1 : 0) * sizeof(uint32_t));
    b->sign = sign; b->n = n;
    return (val)b;
}

val int_normalize(val v) {
    bignum_t *b;
    if (!IS_BIG(v)) return v;
    b = BIG(v);
    while (b->n > 0 && b->limb[b->n - 1] == 0) b->n--;
    if (b->n == 0) return MKFIX(0);
    if (b->n == 1 && b->limb[0] <= (uint32_t)FIXNUM_MAX) return MKFIX(b->sign < 0 ? -(long)b->limb[0] : (long)b->limb[0]);
    if (b->n == 1 && b->sign < 0 && b->limb[0] == (uint32_t)FIXNUM_MAX + 1u) return MKFIX(FIXNUM_MIN);
    return v;
}

val int_from_long(long n) {
    val v;
    unsigned long mag;
    if (FIX_OK(n)) return MKFIX(n);
    mag = n < 0 ? (unsigned long)(-(n + 1)) + 1ul : (unsigned long)n;
    v = mk_bignum(n < 0 ? -1 : 1, 2);
    BIG(v)->limb[0] = (uint32_t)(mag & 0xffffffffu);
    BIG(v)->limb[1] = (uint32_t)((uint64_t)mag >> 32);
    return int_normalize(v);
}

/* Any integer as a (sign, limbs) view, for the fixnum case too. */
typedef struct { int sign; int n; uint32_t tmp[2]; const uint32_t *limb; } iview;

static void view(val v, iview *w) {
    if (IS_FIX(v)) {
        long x = FIXVAL(v);
        unsigned long mag = x < 0 ? (unsigned long)(-(x + 1)) + 1ul : (unsigned long)x;
        w->sign = x < 0 ? -1 : (x == 0 ? 0 : 1);
        w->tmp[0] = (uint32_t)(mag & 0xffffffffu);
        w->tmp[1] = (uint32_t)((uint64_t)mag >> 32);
        w->n = w->tmp[1] ? 2 : (w->tmp[0] ? 1 : 0);
        w->limb = w->tmp;
    } else {
        w->sign = BIG(v)->n == 0 ? 0 : BIG(v)->sign;
        w->n = BIG(v)->n;
        w->limb = BIG(v)->limb;
    }
}

static int mag_cmp(const uint32_t *a, int na, const uint32_t *b, int nb) {
    int i;
    if (na != nb) return na < nb ? -1 : 1;
    for (i = na - 1; i >= 0; i--) if (a[i] != b[i]) return a[i] < b[i] ? -1 : 1;
    return 0;
}

static val mag_add(const uint32_t *a, int na, const uint32_t *b, int nb, int sign) {
    int n = (na > nb ? na : nb) + 1, i;
    val r = mk_bignum(sign, n);
    uint64_t carry = 0;
    for (i = 0; i < n; i++) {
        uint64_t s = carry;
        if (i < na) s += a[i];
        if (i < nb) s += b[i];
        BIG(r)->limb[i] = (uint32_t)s;
        carry = s >> 32;
    }
    return int_normalize(r);
}

/* a - b with |a| >= |b| */
static val mag_sub(const uint32_t *a, int na, const uint32_t *b, int nb, int sign) {
    val r = mk_bignum(sign, na);
    int64_t borrow = 0;
    int i;
    for (i = 0; i < na; i++) {
        int64_t d = (int64_t)a[i] - borrow - (i < nb ? (int64_t)b[i] : 0);
        if (d < 0) { d += ((int64_t)1 << 32); borrow = 1; } else borrow = 0;
        BIG(r)->limb[i] = (uint32_t)d;
    }
    return int_normalize(r);
}

static val int_add(val a, val b) {
    iview x, y;
    long sp = gc_sp;
    val r;
    gc_push(a); gc_push(b);
    view(a, &x); view(b, &y);
    if (x.sign == 0) { gc_sp = sp; return b; }
    if (y.sign == 0) { gc_sp = sp; return a; }
    if (x.sign == y.sign) r = mag_add(x.limb, x.n, y.limb, y.n, x.sign);
    else {
        int c = mag_cmp(x.limb, x.n, y.limb, y.n);
        if (c == 0) r = MKFIX(0);
        else if (c > 0) r = mag_sub(x.limb, x.n, y.limb, y.n, x.sign);
        else r = mag_sub(y.limb, y.n, x.limb, x.n, y.sign);
    }
    gc_sp = sp;
    return r;
}

val int_neg(val v) {
    iview x;
    val r;
    int i;
    if (IS_FIX(v)) return int_from_long(-FIXVAL(v));
    view(v, &x);
    r = mk_bignum(-x.sign, x.n);
    for (i = 0; i < x.n; i++) BIG(r)->limb[i] = x.limb[i];
    return int_normalize(r);
}

static val int_sub(val a, val b) {
    long sp = gc_sp;
    val r;
    gc_push(a);
    b = int_neg(b);
    r = int_add(a, b);
    gc_sp = sp;
    return r;
}

static val int_mul(val a, val b) {
    iview x, y;
    long sp = gc_sp;
    val r;
    int i, j;
    if (IS_FIX(a) && IS_FIX(b)) {
        int64_t p = (int64_t)FIXVAL(a) * (int64_t)FIXVAL(b);
        if (p >= FIXNUM_MIN && p <= FIXNUM_MAX) return MKFIX((long)p);
        if (p >= -(int64_t)0x7fffffffffffLL && p <= (int64_t)0x7fffffffffffLL) {
            /* fits two limbs */
            int sign = p < 0 ? -1 : 1;
            uint64_t mag = p < 0 ? (uint64_t)(-p) : (uint64_t)p;
            r = mk_bignum(sign, 2);
            BIG(r)->limb[0] = (uint32_t)mag; BIG(r)->limb[1] = (uint32_t)(mag >> 32);
            return int_normalize(r);
        }
    }
    gc_push(a); gc_push(b);
    view(a, &x); view(b, &y);
    if (x.sign == 0 || y.sign == 0) { gc_sp = sp; return MKFIX(0); }
    r = mk_bignum(x.sign * y.sign, x.n + y.n);
    memset(BIG(r)->limb, 0, (size_t)(x.n + y.n) * sizeof(uint32_t));
    for (i = 0; i < x.n; i++) {
        uint64_t carry = 0;
        for (j = 0; j < y.n; j++) {
            uint64_t t = (uint64_t)x.limb[i] * y.limb[j] + BIG(r)->limb[i + j] + carry;
            BIG(r)->limb[i + j] = (uint32_t)t;
            carry = t >> 32;
        }
        BIG(r)->limb[i + y.n] += (uint32_t)carry;
    }
    gc_sp = sp;
    return int_normalize(r);
}

/* Truncating division of magnitudes: Knuth D.  q and r are fresh bignums. */
static void mag_divmod(const uint32_t *u, int m, const uint32_t *v, int n, val *qout, val *rout, int qsign, int rsign) {
    val q, r;
    if (n == 1) {
        uint64_t rem = 0;
        int i;
        q = mk_bignum(qsign, m);
        for (i = m - 1; i >= 0; i--) {
            uint64_t cur = (rem << 32) | u[i];
            BIG(q)->limb[i] = (uint32_t)(cur / v[0]);
            rem = cur % v[0];
        }
        r = mk_bignum(rsign, 1);
        BIG(r)->limb[0] = (uint32_t)rem;
        *qout = int_normalize(q); *rout = int_normalize(r);
        return;
    }
    {
        int s = 0, i, j;
        uint32_t *un, *vn;
        uint32_t vtop = v[n - 1];
        while (!(vtop & 0x80000000u)) { vtop <<= 1; s++; }
        un = calloc((size_t)m + 1, sizeof(uint32_t));
        vn = calloc((size_t)n, sizeof(uint32_t));
        for (i = n - 1; i > 0; i--) vn[i] = (v[i] << s) | (s ? v[i - 1] >> (32 - s) : 0);
        vn[0] = v[0] << s;
        un[m] = s ? u[m - 1] >> (32 - s) : 0;
        for (i = m - 1; i > 0; i--) un[i] = (u[i] << s) | (s ? u[i - 1] >> (32 - s) : 0);
        un[0] = u[0] << s;
        q = mk_bignum(qsign, m - n + 1);
        memset(BIG(q)->limb, 0, (size_t)(m - n + 1) * sizeof(uint32_t));
        for (j = m - n; j >= 0; j--) {
            uint64_t num = ((uint64_t)un[j + n] << 32) | un[j + n - 1];
            uint64_t qhat = num / vn[n - 1];
            uint64_t rhat = num % vn[n - 1];
            int64_t borrow, t;
            while (qhat >= ((uint64_t)1 << 32) || qhat * vn[n - 2] > ((rhat << 32) | un[j + n - 2])) {
                qhat--; rhat += vn[n - 1];
                if (rhat >= ((uint64_t)1 << 32)) break;
            }
            borrow = 0;
            {
                uint64_t carry = 0;
                for (i = 0; i < n; i++) {
                    uint64_t p = qhat * vn[i] + carry;
                    carry = p >> 32;
                    t = (int64_t)un[i + j] - borrow - (int64_t)(uint32_t)p;
                    if (t < 0) { t += ((int64_t)1 << 32); borrow = 1; } else borrow = 0;
                    un[i + j] = (uint32_t)t;
                }
                t = (int64_t)un[j + n] - borrow - (int64_t)carry;
                if (t < 0) { t += ((int64_t)1 << 32); borrow = 1; } else borrow = 0;
                un[j + n] = (uint32_t)t;
            }
            BIG(q)->limb[j] = (uint32_t)qhat;
            if (borrow) {
                uint64_t carry = 0;
                BIG(q)->limb[j]--;
                for (i = 0; i < n; i++) {
                    uint64_t sum = (uint64_t)un[i + j] + vn[i] + carry;
                    un[i + j] = (uint32_t)sum;
                    carry = sum >> 32;
                }
                un[j + n] += (uint32_t)carry;
            }
        }
        r = mk_bignum(rsign, n);
        for (i = 0; i < n; i++) BIG(r)->limb[i] = (un[i] >> s) | (s && i + 1 <= m ? (uint64_t)un[i + 1] << (32 - s) : 0);
        free(un); free(vn);
        *qout = int_normalize(q); *rout = int_normalize(r);
    }
}

val int_quotient(val a, val b, val *rem) {
    iview x, y;
    long sp = gc_sp;
    val q, r;
    if (IS_FIX(a) && IS_FIX(b)) {
        long p = FIXVAL(a), d = FIXVAL(b);
        if (rem) *rem = MKFIX(p % d);
        return int_from_long(p / d);
    }
    gc_push(a); gc_push(b);
    view(a, &x); view(b, &y);
    if (y.sign == 0) err_division_by_zero(intern_c("QUOTIENT"), list2(a, b));
    if (mag_cmp(x.limb, x.n, y.limb, y.n) < 0) { gc_sp = sp; if (rem) *rem = a; return MKFIX(0); }
    mag_divmod(x.limb, x.n, y.limb, y.n, &q, &r, x.sign * y.sign, x.sign);
    gc_sp = sp;
    if (rem) *rem = r;
    return q;
}

int int_sign(val v) {
    if (IS_FIX(v)) return FIXVAL(v) < 0 ? -1 : (FIXVAL(v) > 0 ? 1 : 0);
    return BIG(v)->n == 0 ? 0 : BIG(v)->sign;
}

/* floor division: q = floor(a/b), m = a - q*b (sign of b) */
val int_div_floor(val a, val b) {
    val r, q;
    long sp = gc_sp;
    gc_push(a); gc_push(b);
    q = int_quotient(a, b, &r);
    gc_push(q);
    if (int_sign(r) != 0 && int_sign(r) != int_sign(b)) q = int_sub(q, MKFIX(1));
    gc_sp = sp;
    return q;
}

val int_mod_floor(val a, val b) {
    val r;
    long sp = gc_sp;
    gc_push(a); gc_push(b);
    int_quotient(a, b, &r);
    gc_push(r);
    if (int_sign(r) != 0 && int_sign(r) != int_sign(b)) r = int_add(r, b);
    gc_sp = sp;
    return r;
}

static int int_cmp(val a, val b) {
    iview x, y;
    if (IS_FIX(a) && IS_FIX(b)) return FIXVAL(a) < FIXVAL(b) ? -1 : (FIXVAL(a) > FIXVAL(b) ? 1 : 0);
    view(a, &x); view(b, &y);
    if (x.sign != y.sign) return x.sign < y.sign ? -1 : 1;
    {
        int c = mag_cmp(x.limb, x.n, y.limb, y.n);
        return x.sign < 0 ? -c : c;
    }
}

double num_to_double(val v) {
    iview x;
    double d = 0;
    int i;
    if (IS_FLT(v)) return FLT(v)->d;
    if (IS_FIX(v)) return (double)FIXVAL(v);
    view(v, &x);
    for (i = x.n - 1; i >= 0; i--) d = d * 4294967296.0 + x.limb[i];
    return x.sign < 0 ? -d : d;
}

val float_to_integer(double d) {
    val r;
    int neg = d < 0, n = 0, i;
    double m;
    uint32_t limbs[40];
    if (d != d || d == HUGE_VAL || d == -HUGE_VAL) err_domain(mk_float(d), "<integer>");
    d = trunc(d);
    if (fabs(d) < 1e9) return int_from_long((long)d);
    m = fabs(d);
    while (m >= 1.0 && n < 40) {
        double q = floor(m / 4294967296.0);
        limbs[n++] = (uint32_t)(m - q * 4294967296.0);
        m = q;
    }
    r = mk_bignum(neg ? -1 : 1, n);
    for (i = 0; i < n; i++) BIG(r)->limb[i] = limbs[i];
    return int_normalize(r);
}

static void check_num(val v) { if (!IS_NUM(v)) err_domain(v, "<number>"); }

void err_float_underflow(val op, val operands);

static val fp_result(double d, const char *op, val a, val b) {
    double x = num_to_double(a), y = num_to_double(b);
    if ((d == HUGE_VAL || d == -HUGE_VAL) && x != HUGE_VAL && x != -HUGE_VAL && y != HUGE_VAL && y != -HUGE_VAL)
        err_float_overflow(intern_c(op), list2(a, b));
    /* a result that lost its precision to the denormal range, or vanished
     * from two non-zero operands, is an underflow */
    if (x != 0.0 && y != 0.0 && ((d != 0.0 && fabs(d) < 2.2250738585072014e-308) || (d == 0.0 && op[0] != '+' && op[0] != '-')))
        err_float_underflow(intern_c(op), list2(a, b));
    return mk_float(d);
}

val num_add(val a, val b) {
    check_num(a); check_num(b);
    if (IS_FLT(a) || IS_FLT(b)) return fp_result(num_to_double(a) + num_to_double(b), "+", a, b);
    if (IS_FIX(a) && IS_FIX(b)) return int_from_long(FIXVAL(a) + FIXVAL(b));
    return int_add(a, b);
}

val num_sub(val a, val b) {
    check_num(a); check_num(b);
    if (IS_FLT(a) || IS_FLT(b)) return fp_result(num_to_double(a) - num_to_double(b), "-", a, b);
    if (IS_FIX(a) && IS_FIX(b)) return int_from_long(FIXVAL(a) - FIXVAL(b));
    return int_sub(a, b);
}

val num_mul(val a, val b) {
    check_num(a); check_num(b);
    if (IS_FLT(a) || IS_FLT(b)) return fp_result(num_to_double(a) * num_to_double(b), "*", a, b);
    return int_mul(a, b);
}

int num_cmp(val a, val b) {
    check_num(a); check_num(b);
    if (IS_FLT(a) || IS_FLT(b)) {
        double x = num_to_double(a), y = num_to_double(b);
        return x < y ? -1 : (x > y ? 1 : 0);
    }
    return int_cmp(a, b);
}

int num_eq(val a, val b) { return num_cmp(a, b) == 0; }
int num_zerop(val v) { return IS_FLT(v) ? FLT(v)->d == 0.0 : int_sign(v) == 0; }

/* Text of an integer in a radix: repeated division by the largest power
 * of the radix that fits a limb. */
void int_to_text(val v, int radix, char **out, long *len) {
    iview x;
    uint32_t *mag;
    int n, i;
    long cap, pos;
    char *buf;
    static const char digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    view(v, &x);
    if (x.sign == 0) { *out = malloc(2); (*out)[0] = '0'; (*out)[1] = 0; *len = 1; return; }
    n = x.n;
    mag = malloc((size_t)n * sizeof(uint32_t));
    memcpy(mag, x.limb, (size_t)n * sizeof(uint32_t));
    cap = (long)n * 33 + 2;
    buf = malloc((size_t)cap);
    pos = cap - 1;
    buf[pos] = 0;
    while (n > 0) {
        uint64_t rem = 0;
        for (i = n - 1; i >= 0; i--) {
            uint64_t cur = (rem << 32) | mag[i];
            mag[i] = (uint32_t)(cur / (uint32_t)radix);
            rem = cur % (uint32_t)radix;
        }
        buf[--pos] = digits[rem];
        while (n > 0 && mag[n - 1] == 0) n--;
    }
    if (x.sign < 0) buf[--pos] = '-';
    *len = cap - 1 - pos;
    *out = malloc((size_t)*len + 1);
    memcpy(*out, buf + pos, (size_t)*len + 1);
    free(buf); free(mag);
}

val parse_integer_text(const char *s, long len, int radix, int *ok) {
    val acc = MKFIX(0);
    long i;
    long sp = gc_sp;
    long slot;
    *ok = 1;
    if (len == 0) { *ok = 0; return MKFIX(0); }
    slot = gc_push(acc);
    for (i = 0; i < len; i++) {
        int c = (unsigned char)s[i], d;
        if (c >= '0' && c <= '9') d = c - '0';
        else if (c >= 'a' && c <= 'z') d = c - 'a' + 10;
        else if (c >= 'A' && c <= 'Z') d = c - 'A' + 10;
        else { *ok = 0; gc_sp = sp; return MKFIX(0); }
        if (d >= radix) { *ok = 0; gc_sp = sp; return MKFIX(0); }
        acc = int_mul(acc, MKFIX(radix));
        gc_roots[slot] = acc;
        acc = int_add(acc, MKFIX(d));
        gc_roots[slot] = acc;
    }
    gc_sp = sp;
    return acc;
}


/* ---------- builtins (§19.1-19.3) ---------- */

static void need_num(val v) { if (!IS_NUM(v)) err_domain(v, "<number>"); }
static void need_int(val v) { if (!IS_INT(v)) err_domain(v, "<integer>"); }

static val bi_quotient(int argc, val *argv) {
    val acc = argv[0];
    int i;
    long sp = gc_sp;
    long slot = gc_push(acc);
    need_num(acc);
    for (i = 1; i < argc; i++) {
        val d = argv[i];
        need_num(d);
        if (num_zerop(d)) {
            val ops;
            gc_sp = sp;
            ops = list2(argv[0], d);
            err_division_by_zero(intern_c("QUOTIENT"), ops);
        }
        if (IS_INT(acc) && IS_INT(d)) {
            val rem, q = int_quotient(acc, d, &rem);
            if (int_sign(rem) == 0) acc = q;
            else acc = fp_result(num_to_double(acc) / num_to_double(d), "QUOTIENT", acc, d);
        } else {
            acc = fp_result(num_to_double(acc) / num_to_double(d), "QUOTIENT", acc, d);
        }
        gc_roots[slot] = acc;
    }
    gc_sp = sp;
    return acc;
}

static val bi_reciprocal(int argc, val *argv) {
    (void)argc;
    need_num(argv[0]);
    if (num_zerop(argv[0])) err_division_by_zero(intern_c("RECIPROCAL"), list1(argv[0]));
    if (IS_INT(argv[0])) {
        if (IS_FIX(argv[0]) && (FIXVAL(argv[0]) == 1 || FIXVAL(argv[0]) == -1)) return argv[0];
        return fp_result(1.0 / num_to_double(argv[0]), "RECIPROCAL", MKFIX(1), argv[0]);
    }
    if (FLT(argv[0])->d == HUGE_VAL || FLT(argv[0])->d == -HUGE_VAL) err_float_underflow(intern_c("RECIPROCAL"), list1(argv[0]));
    return fp_result(1.0 / FLT(argv[0])->d, "RECIPROCAL", MKFIX(1), argv[0]);
}

static val bi_max(int argc, val *argv) {
    int i;
    val best;
    need_num(argv[0]);
    best = argv[0];
    for (i = 1; i < argc; i++) { need_num(argv[i]); if (num_cmp(argv[i], best) > 0) best = argv[i]; }
    return best;
}
static val bi_min(int argc, val *argv) {
    int i;
    val best;
    need_num(argv[0]);
    best = argv[0];
    for (i = 1; i < argc; i++) { need_num(argv[i]); if (num_cmp(argv[i], best) < 0) best = argv[i]; }
    return best;
}

static val bi_abs(int argc, val *argv) {
    (void)argc;
    need_num(argv[0]);
    if (IS_FLT(argv[0])) return mk_float(fabs(FLT(argv[0])->d));
    return int_sign(argv[0]) < 0 ? int_neg(argv[0]) : argv[0];
}

static val check_float_result(double d, const char *op, val x) {
    if (d == HUGE_VAL || d == -HUGE_VAL) err_float_overflow(intern_c(op), list1(x));
    return mk_float(d);
}

static val bi_exp(int argc, val *argv) {
    double d;
    (void)argc;
    need_num(argv[0]);
    d = exp(num_to_double(argv[0]));
    if (d == 0.0 || (d != 0.0 && fabs(d) < 2.2250738585072014e-308)) err_float_underflow(intern_c("EXP"), list1(argv[0]));
    return check_float_result(d, "EXP", argv[0]);
}
static val bi_log(int argc, val *argv) {
    double d;
    (void)argc;
    need_num(argv[0]);
    d = num_to_double(argv[0]);
    if (d <= 0) err_domain(argv[0], "<number>");
    return mk_float(log(d));
}
static val bi_sin(int argc, val *argv) { (void)argc; need_num(argv[0]); return mk_float(sin(num_to_double(argv[0]))); }
static val bi_cos(int argc, val *argv) { (void)argc; need_num(argv[0]); return mk_float(cos(num_to_double(argv[0]))); }
static val bi_tan(int argc, val *argv) { (void)argc; need_num(argv[0]); return mk_float(tan(num_to_double(argv[0]))); }
static val bi_atan(int argc, val *argv) { (void)argc; need_num(argv[0]); return mk_float(atan(num_to_double(argv[0]))); }
static val bi_atan2(int argc, val *argv) { (void)argc; need_num(argv[0]); need_num(argv[1]); return mk_float(atan2(num_to_double(argv[0]), num_to_double(argv[1]))); }
static val bi_sinh(int argc, val *argv) { (void)argc; need_num(argv[0]); return check_float_result(sinh(num_to_double(argv[0])), "SINH", argv[0]); }
static val bi_cosh(int argc, val *argv) { (void)argc; need_num(argv[0]); return check_float_result(cosh(num_to_double(argv[0])), "COSH", argv[0]); }
static val bi_tanh(int argc, val *argv) { (void)argc; need_num(argv[0]); return mk_float(tanh(num_to_double(argv[0]))); }
static val bi_atanh(int argc, val *argv) {
    double d;
    (void)argc;
    need_num(argv[0]);
    d = num_to_double(argv[0]);
    if (d <= -1.0 || d >= 1.0) err_domain(argv[0], "<number>");
    return mk_float(0.5 * log((1.0 + d) / (1.0 - d)));
}

/* integer square root by Newton's method on integers */
static val isqrt_int(val n) {
    val x, y, two = MKFIX(2);
    long sp = gc_sp;
    long xs, ys;
    if (int_sign(n) == 0) return MKFIX(0);
    if (IS_FIX(n)) {
        long v = FIXVAL(n);
        long r = (long)sqrt((double)v);
        while (r * r > v) r--;
        while ((r + 1) * (r + 1) <= v) r++;
        return MKFIX(r);
    }
    /* a starting point above the root: 2^(bits/2 + 1) */
    {
        int bits = BIG(n)->n * 32;
        int k = bits / 2 + 1, i;
        x = MKFIX(1);
        xs = gc_push(x); ys = gc_push(NIL); gc_push(n);
        for (i = 0; i < k; i++) { x = int_mul(x, two); gc_roots[xs] = x; }
    }
    for (;;) {
        y = int_quotient(n, x, NULL);
        gc_roots[ys] = y;
        y = int_add(x, y);
        gc_roots[ys] = y;
        y = int_quotient(y, two, NULL);
        gc_roots[ys] = y;
        if (num_cmp(y, x) >= 0) break;
        x = y;
        gc_roots[xs] = x;
    }
    gc_sp = sp;
    return x;
}

static val bi_isqrt(int argc, val *argv) {
    (void)argc;
    need_int(argv[0]);
    if (int_sign(argv[0]) < 0) err_domain(argv[0], "<integer>");
    return isqrt_int(argv[0]);
}

static val bi_sqrt(int argc, val *argv) {
    (void)argc;
    need_num(argv[0]);
    if (IS_INT(argv[0])) {
        val r, sq;
        long sp = gc_sp;
        if (int_sign(argv[0]) < 0) err_domain(argv[0], "<number>");
        r = isqrt_int(argv[0]);
        gc_push(r);
        sq = int_mul(r, r);
        gc_sp = sp;
        if (num_cmp(sq, argv[0]) == 0) return r;
        return mk_float(sqrt(num_to_double(argv[0])));
    }
    if (FLT(argv[0])->d < 0) err_domain(argv[0], "<number>");
    return mk_float(sqrt(FLT(argv[0])->d));
}

static val bi_expt(int argc, val *argv) {
    val x = argv[0], y = argv[1];
    (void)argc;
    need_num(x); need_num(y);
    if (IS_INT(x) && IS_INT(y)) {
        if (int_sign(y) >= 0) {
            /* integer power by squaring; the exponent fits a long in any sane program */
            val result = MKFIX(1), base = x;
            long e;
            long sp = gc_sp;
            long rs, bs;
            if (!IS_FIX(y)) {
                if (IS_FIX(x) && (FIXVAL(x) == 0 || FIXVAL(x) == 1)) return x;
                if (IS_FIX(x) && FIXVAL(x) == -1) return int_mod_floor(y, MKFIX(2)) == MKFIX(0) ? MKFIX(1) : MKFIX(-1);
                isl_error("<STORAGE-EXHAUSTED>", NULL);
            }
            e = FIXVAL(y);
            rs = gc_push(result); bs = gc_push(base);
            while (e > 0) {
                if (e & 1) { result = int_mul(result, base); gc_roots[rs] = result; }
                e >>= 1;
                if (e) { base = int_mul(base, base); gc_roots[bs] = base; }
            }
            gc_sp = sp;
            return result;
        }
        if (int_sign(x) == 0) err_division_by_zero(intern_c("EXPT"), list2(x, y));
        return fp_result(pow(num_to_double(x), num_to_double(y)), "EXPT", x, y);
    }
    {
        double a = num_to_double(x), b = num_to_double(y);
        if (a == 0.0 && b < 0) err_division_by_zero(intern_c("EXPT"), list2(x, y));
        if (a == 0.0 && IS_FLT(y) && b == 0.0) err_domain(y, "<number>");
        if (a < 0 && !IS_INT(y)) err_domain(y, "<number>");
        if (a == 0.0) return mk_float(b == 0.0 ? 1.0 : 0.0);
        return fp_result(pow(a, b), "EXPT", x, y);
    }
}

static val bi_float(int argc, val *argv) { (void)argc; need_num(argv[0]); return IS_FLT(argv[0]) ? argv[0] : mk_float(num_to_double(argv[0])); }

static val round_with(val x, double (*f)(double)) {
    need_num(x);
    if (IS_INT(x)) return x;
    return float_to_integer(f(FLT(x)->d));
}
static double round_even(double d) {
    double r = floor(d), diff = d - r;
    if (diff > 0.5) return r + 1;
    if (diff < 0.5) return r;
    return fmod(r, 2.0) == 0 ? r : r + 1;
}
static val bi_floor(int argc, val *argv) { (void)argc; return round_with(argv[0], floor); }
static val bi_ceiling(int argc, val *argv) { (void)argc; return round_with(argv[0], ceil); }
static val bi_truncate(int argc, val *argv) { (void)argc; return round_with(argv[0], trunc); }
static val bi_round(int argc, val *argv) { (void)argc; return round_with(argv[0], round_even); }

static val bi_div(int argc, val *argv) {
    (void)argc;
    need_int(argv[0]); need_int(argv[1]);
    if (int_sign(argv[1]) == 0) err_division_by_zero(intern_c("DIV"), list2(argv[0], argv[1]));
    return int_div_floor(argv[0], argv[1]);
}
static val bi_mod(int argc, val *argv) {
    (void)argc;
    need_int(argv[0]); need_int(argv[1]);
    if (int_sign(argv[1]) == 0) err_division_by_zero(intern_c("MOD"), list2(argv[0], argv[1]));
    return int_mod_floor(argv[0], argv[1]);
}

static val int_gcd(val a, val b) {
    long sp = gc_sp;
    long as, bs;
    if (int_sign(a) < 0) a = int_neg(a);
    as = gc_push(a);
    if (int_sign(b) < 0) b = int_neg(b);
    bs = gc_push(b);
    while (int_sign(b) != 0) {
        val r;
        int_quotient(a, b, &r);
        a = b; gc_roots[as] = a;
        b = r; gc_roots[bs] = b;
    }
    gc_sp = sp;
    return a;
}
static val bi_gcd(int argc, val *argv) { (void)argc; need_int(argv[0]); need_int(argv[1]); return int_gcd(argv[0], argv[1]); }
static val bi_lcm(int argc, val *argv) {
    val g, p;
    long sp = gc_sp;
    (void)argc;
    need_int(argv[0]); need_int(argv[1]);
    if (int_sign(argv[0]) == 0 || int_sign(argv[1]) == 0) return MKFIX(0);
    g = int_gcd(argv[0], argv[1]);
    gc_push(g);
    p = int_mul(argv[0], argv[1]);
    gc_push(p);
    if (int_sign(p) < 0) p = int_neg(p);
    gc_push(p);
    p = int_quotient(p, g, NULL);
    gc_sp = sp;
    return p;
}

static val bi_parse_number(int argc, val *argv) {
    val v;
    const char *s;
    long n;
    (void)argc;
    if (!IS_STR(argv[0])) err_domain(argv[0], "<string>");
    s = STR(argv[0])->data; n = STR(argv[0])->len;
    /* #B #O #X radix forms are lexemes too */
    if (n > 2 && s[0] == '#') {
        int radix = (s[1] == 'b' || s[1] == 'B') ? 2 : (s[1] == 'o' || s[1] == 'O') ? 8 : (s[1] == 'x' || s[1] == 'X') ? 16 : 0;
        int ok, neg = 0;
        long i = 2;
        if (radix) {
            if (i < n && (s[i] == '+' || s[i] == '-')) { neg = s[i] == '-'; i++; }
            v = parse_integer_text(s + i, n - i, radix, &ok);
            if (ok && i < n) return neg ? int_neg(v) : v;
        }
    }
    if (!parse_number_text(s, n, &v)) err_parse(argv[0], "<NUMBER>");
    return v;
}

void number_init(void) {
    val s;
    defbuiltin("QUOTIENT", bi_quotient, 2, -1);
    defbuiltin("RECIPROCAL", bi_reciprocal, 1, 1);
    defbuiltin("MAX", bi_max, 1, -1);
    defbuiltin("MIN", bi_min, 1, -1);
    defbuiltin("ABS", bi_abs, 1, 1);
    defbuiltin("EXP", bi_exp, 1, 1);
    defbuiltin("LOG", bi_log, 1, 1);
    defbuiltin("EXPT", bi_expt, 2, 2);
    defbuiltin("SQRT", bi_sqrt, 1, 1);
    defbuiltin("ISQRT", bi_isqrt, 1, 1);
    defbuiltin("SIN", bi_sin, 1, 1);
    defbuiltin("COS", bi_cos, 1, 1);
    defbuiltin("TAN", bi_tan, 1, 1);
    defbuiltin("ATAN", bi_atan, 1, 1);
    defbuiltin("ATAN2", bi_atan2, 2, 2);
    defbuiltin("SINH", bi_sinh, 1, 1);
    defbuiltin("COSH", bi_cosh, 1, 1);
    defbuiltin("TANH", bi_tanh, 1, 1);
    defbuiltin("ATANH", bi_atanh, 1, 1);
    defbuiltin("FLOAT", bi_float, 1, 1);
    defbuiltin("FLOOR", bi_floor, 1, 1);
    defbuiltin("CEILING", bi_ceiling, 1, 1);
    defbuiltin("TRUNCATE", bi_truncate, 1, 1);
    defbuiltin("ROUND", bi_round, 1, 1);
    defbuiltin("DIV", bi_div, 2, 2);
    defbuiltin("MOD", bi_mod, 2, 2);
    defbuiltin("GCD", bi_gcd, 2, 2);
    defbuiltin("LCM", bi_lcm, 2, 2);
    defbuiltin("PARSE-NUMBER", bi_parse_number, 1, 1);
    s = intern_c("*PI*"); SYM(s)->value = mk_float(3.14159265358979323846); SYM(s)->constant = 2;
    s = intern_c("*MOST-POSITIVE-FLOAT*"); SYM(s)->value = mk_float(1.7976931348623157e308); SYM(s)->constant = 2;
    s = intern_c("*MOST-NEGATIVE-FLOAT*"); SYM(s)->value = mk_float(-1.7976931348623157e308); SYM(s)->constant = 2;
    /* not ISLISP: the oracle's names for the IEEE infinities, which the suite uses */
    s = intern_c("*POSITIVE-INFINITY*"); SYM(s)->value = mk_float(HUGE_VAL); SYM(s)->constant = 1;
    s = intern_c("*NEGATIVE-INFINITY*"); SYM(s)->value = mk_float(-HUGE_VAL); SYM(s)->constant = 1;
}
