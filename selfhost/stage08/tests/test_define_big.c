/* GitHub issue 60: a numeric #define whose body is a plain number past
 * INT_MAX must type as long long, the same as the unsuffixed literal.
 * The 10-digit body is stored as text (nd > 9) and re-lexed, so
 * lex_val_macro is 0 and the lexer promotes it.  #define N -5 must
 * stay a signed int (GitHub issue 40 / 89fdbbbc).
 * #if arithmetic is still 32-bit; #if BIG > 0 is not this pin. */
#define BIG 2147483648
#define NEG -5
#define FITS 2147483647
#define BILLION 1000000000
#define HEXBIG 0x80000000

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
    return 0;
}
