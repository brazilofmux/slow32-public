/* test_phase33.c -- file-scope long long initializers (GitHub #11)
 *
 * A file-scope long long array initializer repeated each element's
 * 32-bit encoding to fill the 8-byte slot, and a long long global's
 * initializer lost any high word: the constant evaluator is 32-bit and
 * ps_ginit_store_int_at shifted a 32-bit value by 32.  libcob's scale
 * table `static const long long pow10tab[19]` is exactly that shape, so
 * every COBOL division returned 0 on a host that builds libcob with
 * this compiler.  Every case here is C89. */

static const long long ga[3] = { 1LL, 10LL, -5LL };
static long long gs = 7LL;
static long long gneg = -7LL;
static const long long gbig[2] = { 5000000000LL, -5000000000LL };
static long long gbigs = 5000000000LL;
static long long gbign = -5000000000LL;
static const unsigned long long gu[2] = { 1ULL, 10ULL };
static const int gi[3] = { 1, 10, -5 };
static const long long gexpr[2] = { 3 * 7, -(2 + 3) };
static const long long gpow[5] = { 1LL, 10LL, 100LL, 1000LL, 10000000000LL };

int failed;

void check(int cond, int code) {
    if (!cond && failed == 0) failed = code;
}

int main(void) {
    long long la[3] = { 1LL, 10LL, -5LL };
    long long big;
    big = 5000000000LL;
    check(ga[0] == 1 && ga[1] == 10 && ga[2] == -5, 1);
    check(gs == 7, 2);
    check(gneg == -7, 3);
    check(gbig[0] == big && gbig[1] == -big, 4);
    check(gbigs == big, 5);
    check(gbign == -big, 6);
    check(gu[0] == 1 && gu[1] == 10, 7);
    check(gi[0] == 1 && gi[1] == 10 && gi[2] == -5, 8);
    check(gexpr[0] == 21 && gexpr[1] == -5, 9);
    check(gpow[4] == 10000000000LL, 10);
    check(gpow[4] / gpow[1] == 1000000000LL, 11);
    check(la[0] == 1 && la[1] == 10 && la[2] == -5, 12);
    check((int)(gbig[0] >> 32) == 1 && (int)(gbig[1] >> 32) == -2, 13);
    return failed;
}
