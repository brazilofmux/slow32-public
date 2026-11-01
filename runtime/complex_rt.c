/* Complex multiplication and division, the support routines the compilers
 * call (__mulsc3, __muldc3, __divsc3, __divdc3: the names and arguments
 * compiler-rt and libgcc use, real and imaginary parts of each operand),
 * for both C libraries.
 *
 * clang multiplies inline and calls __mul?c3 only when both parts of the
 * product came out NaN; it divides by calling __div?c3.  stage08 calls
 * both always.  The product here is computed exactly as clang's inline
 * code computes it -- ac - bd, ad + bc, nothing contracted into a fused
 * multiply-add -- so the two compilers' programs agree to the bit.
 *
 * The straightforward formulas lose infinities: (inf + 0i)(1 + 0i) has
 * ac - bd = inf - nan.  When both parts are NaN, an operand that is
 * infinite in either part is taken as a unit in its direction (each part
 * +-1 where infinite, +-0 where finite, NaN parts made +-0) and the
 * product or quotient is recomputed, scaled by infinity or by zero, so an
 * infinite operand gives an infinite result (C11 G.5.1's properties).
 * Division scales the divisor by a power of two near its magnitude first,
 * so that c*c + d*d neither overflows nor underflows for representable
 * operands, and forms each numerator, ac + bd and bc - ad, with one
 * rounding (Kahan's fused-multiply-add difference): the plain form loses
 * the low bits to cancellation, and (1e-300 + 2e-300i) / (3e-300 +
 * 4e-300i) came out two units in the last place from 0.08i.  No compiler
 * divides inline, so this costs no agreement. */
#include <math.h>

#pragma STDC FP_CONTRACT OFF

/* +-1 for an infinity, +-0 otherwise: the sign kept */
static double unit_d(double x) {
    return copysign(isinf(x) ? 1.0 : 0.0, x);
}

static float unit_f(float x) {
    return copysignf(isinf(x) ? 1.0f : 0.0f, x);
}

/* NaN replaced by a zero of its sign; anything else as it is */
static double denan_d(double x) {
    return isnan(x) ? copysign(0.0, x) : x;
}

static float denan_f(float x) {
    return isnan(x) ? copysignf(0.0f, x) : x;
}

/* x*y + z*w, rounded once (Kahan): the product z*w's rounding error is
 * recovered with fma and added back -- when z*w is finite; an infinite
 * one has no error to recover, and inf - inf would make it NaN */
static double dot2_d(double x, double y, double z, double w) {
    double p = z * w;
    double e = isfinite(p) ? fma(z, w, -p) : 0.0;
    return fma(x, y, p) + e;
}

static float dot2_f(float x, float y, float z, float w) {
    float p = z * w;
    float e = isfinite(p) ? fmaf(z, w, -p) : 0.0f;
    return fmaf(x, y, p) + e;
}

union cd { double _Complex z; double p[2]; };
union cf { float _Complex z; float p[2]; };

double _Complex __muldc3(double a, double b, double c, double d) {
    double rr = a * c, ii = b * d, ri = a * d, ir = b * c;   /* the four partial products */
    union cd r;
    r.p[0] = rr - ii;
    r.p[1] = ri + ir;
    if (isnan(r.p[0]) && isnan(r.p[1])) {
        int again = 0;
        if (isinf(a) || isinf(b)) {
            a = unit_d(a);
            b = unit_d(b);
            c = denan_d(c);
            d = denan_d(d);
            again = 1;
        }
        if (isinf(c) || isinf(d)) {
            c = unit_d(c);
            d = unit_d(d);
            a = denan_d(a);
            b = denan_d(b);
            again = 1;
        }
        if (!again && (isinf(rr) || isinf(ii) || isinf(ri) || isinf(ir))) {
            /* finite operands whose partial products overflowed */
            a = denan_d(a);
            b = denan_d(b);
            c = denan_d(c);
            d = denan_d(d);
            again = 1;
        }
        if (again) {
            r.p[0] = INFINITY * (a * c - b * d);
            r.p[1] = INFINITY * (a * d + b * c);
        }
    }
    return r.z;
}

float _Complex __mulsc3(float a, float b, float c, float d) {
    float rr = a * c, ii = b * d, ri = a * d, ir = b * c;   /* the four partial products */
    union cf r;
    r.p[0] = rr - ii;
    r.p[1] = ri + ir;
    if (isnan(r.p[0]) && isnan(r.p[1])) {
        int again = 0;
        if (isinf(a) || isinf(b)) {
            a = unit_f(a);
            b = unit_f(b);
            c = denan_f(c);
            d = denan_f(d);
            again = 1;
        }
        if (isinf(c) || isinf(d)) {
            c = unit_f(c);
            d = unit_f(d);
            a = denan_f(a);
            b = denan_f(b);
            again = 1;
        }
        if (!again && (isinf(rr) || isinf(ii) || isinf(ri) || isinf(ir))) {
            a = denan_f(a);
            b = denan_f(b);
            c = denan_f(c);
            d = denan_f(d);
            again = 1;
        }
        if (again) {
            r.p[0] = INFINITY * (a * c - b * d);
            r.p[1] = INFINITY * (a * d + b * c);
        }
    }
    return r.z;
}

double _Complex __divdc3(double a, double b, double c, double d) {
    double lw = logb(fmax(fabs(c), fabs(d)));
    int scale = 0;
    double den;
    union cd r;
    if (isfinite(lw)) {
        scale = (int)lw;
        c = scalbn(c, -scale);
        d = scalbn(d, -scale);
    }
    den = c * c + d * d;
    r.p[0] = scalbn(dot2_d(a, c, b, d) / den, -scale);
    r.p[1] = scalbn(dot2_d(b, c, -a, d) / den, -scale);
    if (isnan(r.p[0]) && isnan(r.p[1])) {
        double huge = copysign(INFINITY, c);
        int top_inf = isinf(a) || isinf(b), top_fin = isfinite(a) && isfinite(b);
        int bot_fin = isfinite(c) && isfinite(d), bot_inf = isinf(lw) && lw > 0.0;
        if (den == 0.0 && (!isnan(a) || !isnan(b))) {
            /* a nonzero (or infinite) number over zero */
            r.p[0] = huge * a;
            r.p[1] = huge * b;
        } else if (top_inf && bot_fin) {
            a = unit_d(a);
            b = unit_d(b);
            r.p[0] = INFINITY * (a * c + b * d);
            r.p[1] = INFINITY * (b * c - a * d);
        } else if (bot_inf && top_fin) {
            /* a finite number over an infinite one */
            c = unit_d(c);
            d = unit_d(d);
            r.p[0] = 0.0 * (a * c + b * d);
            r.p[1] = 0.0 * (b * c - a * d);
        }
    }
    return r.z;
}

float _Complex __divsc3(float a, float b, float c, float d) {
    float lw = logbf(fmaxf(fabsf(c), fabsf(d)));
    int scale = 0;
    float den;
    union cf r;
    if (isfinite(lw)) {
        scale = (int)lw;
        c = scalbnf(c, -scale);
        d = scalbnf(d, -scale);
    }
    den = c * c + d * d;
    r.p[0] = scalbnf(dot2_f(a, c, b, d) / den, -scale);
    r.p[1] = scalbnf(dot2_f(b, c, -a, d) / den, -scale);
    if (isnan(r.p[0]) && isnan(r.p[1])) {
        float huge = copysignf(INFINITY, c);
        int top_inf = isinf(a) || isinf(b), top_fin = isfinite(a) && isfinite(b);
        int bot_fin = isfinite(c) && isfinite(d), bot_inf = isinf(lw) && lw > 0.0f;
        if (den == 0.0f && (!isnan(a) || !isnan(b))) {
            r.p[0] = huge * a;
            r.p[1] = huge * b;
        } else if (top_inf && bot_fin) {
            a = unit_f(a);
            b = unit_f(b);
            r.p[0] = INFINITY * (a * c + b * d);
            r.p[1] = INFINITY * (b * c - a * d);
        } else if (bot_inf && top_fin) {
            c = unit_f(c);
            d = unit_f(d);
            r.p[0] = 0.0f * (a * c + b * d);
            r.p[1] = 0.0f * (b * c - a * d);
        }
    }
    return r.z;
}
