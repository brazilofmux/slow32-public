/* GitHub issue 60: a numeric #define whose body is a plain number past
 * INT_MAX must type as long long, the same as the unsuffixed literal.
 * The 10-digit body is stored as text (nd > 9) and re-lexed, so
 * lex_val_macro is 0 and the lexer promotes it.  #define N -5 must
 * stay a signed int (GitHub issue 40 / 89fdbbbc).
 * #if arithmetic is long long: #if BIG > 0 is true (it wrapped to
 * -2147483648 as a 32-bit int).  defined() in a sum must still be 1
 * (dtoa's IEEE_8087 check) now that stage07 wraps long-long returns. */
#define BIG 2147483648
#define NEG -5
#define FITS 2147483647
#define BILLION 1000000000
#define HEXBIG 0x80000000
#define IEEE_8087

#if BIG > 0
int big_pos = 1;
#else
int big_pos = 0;
#endif
#if HEXBIG > 0
int hex_pos = 1;
#else
int hex_pos = 0;
#endif
#if NEG < 0
int neg_ok = 1;
#else
int neg_ok = 0;
#endif
#if 2147483648 > 0
int lit_pos = 1;
#else
int lit_pos = 0;
#endif
#if defined(IEEE_8087) + defined(IEEE_MC68k) + defined(VAX) + defined(IBM) == 1
int ieee_ok = 1;
#else
int ieee_ok = 0;
#endif
#if (1 << 32) != 0
int shl32_ok = 1;
#else
int shl32_ok = 0;
#endif

static long long a = BIG;
static long long b = 2147483648;
static int n = NEG;
static int f = FITS;
static int g = BILLION;
static unsigned h = HEXBIG;

int main(void) {
    if (a != 2147483648LL) return 1;
    if (b != 2147483648LL) return 2;
    if (a != b) return 3;
    if (n != -5) return 4;
    if (f != 2147483647) return 5;
    if (g != 1000000000) return 6;
    if (h != 0x80000000u) return 7;
    if (!(BIG > -1)) return 8;
    if (!big_pos) return 9;
    if (!hex_pos) return 10;
    if (!neg_ok) return 11;
    if (!lit_pos) return 12;
    if (!ieee_ok) return 13;
    if (!shl32_ok) return 14;
    return 0;
}
