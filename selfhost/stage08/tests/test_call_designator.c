/* A call whose callee is an expression designating a function, not a
 * name or a function-pointer variable: (f)(x), and what _Generic
 * selects -- every <tgmath.h> call is one.  The result took int (a float
 * came back as its bits) and the arguments went unconverted (an int
 * reached a double parameter as is) (libc-conformance 7.25-tgmath). */
double ext_dbl(double x);
float ext_flt(float x);
static double dbl(double x) { return x * 2; }
static float flt(float x) { return x * 3; }
int main(void) {
    int i = 9;
    float f = 2.0f;
    float (*pf)(float) = flt;
    if ((dbl)(i) != 18) return 1;
    if ((flt)(f) != 6) return 2;
    if ((ext_flt)(f) != 14) return 3;
    if (_Generic(i, int: dbl)(i) != 18) return 4;
    if (_Generic(f, float: flt, default: dbl)(f) != 6) return 5;
    if (_Generic((double)0, float: flt, default: ext_dbl)(i) != 27) return 6;
    if ((1 ? dbl : dbl)(i) != 18) return 7;
    if (pf(f) != 6) return 8;
    return 0;
}
double ext_dbl(double x) { return x * 3; }
float ext_flt(float x) { return x * 7; }
