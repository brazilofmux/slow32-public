/* A cast or a sizeof of a type name inside an array's dimension: reading
 * that type name ran parse_type again, and the declaration lost what it
 * had left in parse_type's globals -- const char *a[(int)2] was an array
 * of char *, and _Alignas and a function-pointer typedef's signature went
 * the same way.  File scope, static block scope and automatic block scope
 * take three different paths to the dimension. */
typedef double (*dfn)(double);
static double twice(double x) { return x * 2; }

const char *names[(int)2] = { "a", "b" };
const char *more[sizeof(int)];
dfn table[(unsigned char)3] = { twice, twice, twice };
_Alignas(64) char aligned[(int)8];

_Static_assert(_Generic(&names[0], const char **: 1, default: 0), "file scope, cast");
_Static_assert(_Generic(&more[0], const char **: 1, default: 0), "file scope, sizeof");
_Static_assert(sizeof more == 4 * sizeof(char *), "file scope, sizeof count");

int main(void) {
    static const char *sn[(int)2];
    const char *an[(long)3];
    const char *bn[sizeof(short)][(int)2];
    dfn local[(int)2] = { twice, twice };
    _Alignas(32) char abuf[(short)4];
    _Static_assert(_Generic(&sn[0], const char **: 1, default: 0), "static block scope");
    _Static_assert(_Generic(&an[0], const char **: 1, default: 0), "block scope");
    _Static_assert(_Generic(&bn[0][0], const char **: 1, default: 0), "two dimensions");
    _Static_assert(sizeof an == 3 * sizeof(char *), "count");
    if (table[2](1.5) != 3.0) return 1;
    if (local[1](2.25) != 4.5) return 2;
    if (((unsigned long)aligned & 63) != 0) return 3;
    if (((unsigned long)abuf & 31) != 0) return 4;
    (void)sn; (void)an; (void)bn;
    return 0;
}
