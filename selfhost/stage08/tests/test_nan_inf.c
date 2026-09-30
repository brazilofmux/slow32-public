/* selfhost ISSUES-71: __builtin_nan[f], __builtin_inf[f] and
 * __builtin_huge_val[f] fold to their constants, as clang folds them;
 * runtime/include/math.h's NAN and INFINITY are these.  They were calls
 * to functions that do not exist, found at the link. */
static double dnan = __builtin_nan("");
static float fnan = __builtin_nanf("");
static double dinf = __builtin_inf();
static float finf = __builtin_inff();
static double dhuge = __builtin_huge_val();

typedef union { double d; unsigned w[2]; } dbits;
typedef union { float f; unsigned w; } fbits;

int main(void) {
    dbits a; fbits b;
    a.d = dnan; if (a.w[1] != 0x7FF80000u || a.w[0] != 0) return 1;
    b.f = fnan; if (b.w != 0x7FC00000u) return 2;
    a.d = dinf; if (a.w[1] != 0x7FF00000u || a.w[0] != 0) return 3;
    b.f = finf; if (b.w != 0x7F800000u) return 4;
    if (dhuge != dinf) return 5;
    if (dnan == dnan) return 6;                 /* a NaN is unequal to itself */
    if (!(dinf > 1e308)) return 7;
    {   double x = __builtin_inf() - __builtin_inf();   /* in an expression, not only an initializer */
        if (x == x) return 8;
        if (-__builtin_huge_valf() >= -3.0e38f) return 9;
    }
    return 0;
}
