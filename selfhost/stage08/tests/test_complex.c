/* _Complex (6.2.5p11-13, 6.3.1.6-8, 6.5): the structure of two parts it
 * is to the machine, and sema's rewrite of its arithmetic into real
 * arithmetic on the parts (sema.h sema_cplx).  Products and quotients of
 * two complex numbers call runtime/complex_rt.c, which this test's small
 * library has not; real-by-complex ones are inline, and are here. */
typedef double _Complex dc;
typedef float _Complex fc;

dc g1 = __builtin_complex(1.5, -2.0);
dc g2 = 3.0;
dc g3 = { 7.0, 8.0 };
fc gf = __builtin_complex(2.5f, -0.5f);
dc garr[2] = { __builtin_complex(1.0, 2.0), 4.0 };
struct S { int tag; dc z; } gs = { 9, __builtin_complex(1.0, -1.0) };

static int calls;
static int idx(int i) { calls = calls + 1; return i; }
static dc twice(dc z) { return z + z; }
static double re(dc z) { return z; }    /* the real part, by return */

int main(void) {
    dc a = __builtin_complex(1.25, -0.5);
    dc b = __builtin_complex(-2.0, 3.0);
    dc c;
    dc arr[3] = { 1.0, 2.0, 3.0 };
    fc f = gf;
    double d;
    int k;
    _Bool t;
    static dc st = __builtin_complex(9.0, 10.0);
    if (sizeof(dc) != 16 || sizeof(fc) != 8) return 1;
    if (__real__ g1 != 1.5 || __imag__ g1 != -2.0) return 2;
    if (__real__ g2 != 3.0 || __imag__ g2 != 0.0) return 3;
    if (__real__ g3 != 7.0 || __imag__ g3 != 8.0) return 4;
    if (__real__ garr[1] != 4.0 || __imag__ garr[0] != 2.0) return 5;
    if (gs.tag != 9 || __imag__ gs.z != -1.0) return 6;
    if (__real__ st != 9.0 || __imag__ st != 10.0) return 7;
    c = a + b;
    if (__real__ c != -0.75 || __imag__ c != 2.5) return 8;
    c = a - b;
    if (__real__ c != 3.25 || __imag__ c != -3.5) return 9;
    c = 2.0 * a;
    if (__real__ c != 2.5 || __imag__ c != -1.0) return 10;
    c = b / 2.0;
    if (__real__ c != -1.0 || __imag__ c != 1.5) return 11;
    c = -a;
    if (__real__ c != -1.25 || __imag__ c != 0.5) return 12;
    c = 1.0 - b;                    /* the imaginary part negated */
    if (__real__ c != 3.0 || __imag__ c != -3.0) return 13;
    arr[idx(1)] += a;               /* once */
    if (calls != 1 || __real__ arr[1] != 3.25 || __imag__ arr[1] != -0.5) return 14;
    arr[0] *= 3.0;
    if (__real__ arr[0] != 3.0 || __imag__ arr[0] != 0.0) return 15;
    d = 1.0;
    d += b;                         /* d = d + b, its real part */
    if (d != -1.0) return 16;
    k = 7;
    k -= a;
    if (k != 5) return 17;
    if (!a) return 18;
    if (__builtin_complex(0.0, 0.0)) return 19;
    t = __builtin_complex(0.0, 0.5);
    if (!t) return 20;
    if (a == b || !(a == a) || !(a != b)) return 21;
    if (g2 != 3.0) return 22;
    if ((a && b) != 1 || (a && __builtin_complex(0.0, 0.0)) != 0) return 23;
    c = 0 ? a : 4.0;
    if (__real__ c != 4.0 || __imag__ c != 0.0) return 24;
    k = (int)__builtin_complex(-3.75, 9.0);
    if (k != -3) return 25;
    c = twice(a);
    if (__real__ c != 2.5 || __imag__ c != -1.0) return 26;
    if (re(b) != -2.0) return 27;
    __real__ c = 11.0;
    __imag__ c = 12.0;
    if (__real__ c != 11.0 || __imag__ c != 12.0) return 28;
    if (__imag__ (a + b) != 2.5) return 29;
    c = f;                          /* float complex to double complex */
    if (__real__ c != 2.5 || __imag__ c != -0.5) return 30;
    f = a;
    if (__real__ f != 1.25f) return 31;
    if (_Generic(a, dc: 1, default: 0) != 1 || _Generic(f + 1.0, dc: 1, default: 0) != 1) return 32;
    return 0;
}
