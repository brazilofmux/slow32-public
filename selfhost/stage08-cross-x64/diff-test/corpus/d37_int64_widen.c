/* 32-bit values flowing into 64-bit slots on the 64-bit hosts (GitHub
 * issue 82, found when the self-hosted DBT first ran through cc-a64
 * again).  Three shapes, all wrong before the fix on both cc-x64 and
 * cc-a64 and all correct on SLOW-32, whose long long is a word pair:
 *
 *   1. p - q was a 32-bit `sub w` although sema types it as a pointer,
 *      so (long long)(p - q) had a zero upper half and >> 2 of a
 *      negative difference came out as an unsigned shift.
 *   2. (int)(ll >> 2) kept its 64-bit producer, and the back ends size
 *      a compare by its operands' producers, so a following
 *      `< -(1 << 25)` was a 64-bit compare against a zero-extended
 *      32-bit constant.
 *   3. long long d = -16 (and d = x, d += x, a mixed ternary, return x
 *      from a long long function) stored the int without widening it:
 *      0x00000000FFFFFFF0.  */
int printf(char *fmt, ...);

struct S { int a; long long m; };
static long long g64;

static void show64(char *label, long long v)
{
    printf("%s %08x%08x\n", label,
           (unsigned)((unsigned long long)v >> 32), (unsigned)v);
}

static int m16(void) { return -16; }
static unsigned int u16(void) { return 0xFFFFFFF0u; }
static signed char c16(void) { return (signed char)-16; }   /* plain char is unsigned on AArch64 */
static unsigned char *pp(unsigned char *p) { return p; }
static int *ip(int *p) { return p; }

/* Shape 1 */
static long long pdiff(unsigned char *a, unsigned char *b) { return (long long)(b - a); }
static int pdiff_shr(unsigned char *a, unsigned char *b) { long long d = (long long)(b - a); return (int)(d >> 2); }
static long long idiff(int *a, int *b) { return b - a; }
/* the DBT's chain-patch range check */
static int in_range(unsigned char *site, unsigned char *target)
{
    long long diff = (long long)(target - site);
    int imm26 = (int)(diff >> 2);
    if (imm26 < -(1 << 25) || imm26 >= (1 << 25)) return 0;
    return 1;
}
/* Shape 2 */
static int trunc_cmp(long long d) { int i = (int)(d >> 2); return i < -(1 << 25); }
static int trunc_val(long long d) { return (int)(d >> 2); }
/* Shape 3 */
static long long init_const(void) { long long d = -16; return d; }
static long long init_call(void) { long long d = m16(); return d; }
static long long init_param(int x) { long long d = x; return d; }
static long long assign(void) { long long d; d = -16; return d; }
static long long global(void) { g64 = m16(); return g64; }
static long long member(void) { struct S s; s.m = m16(); return s.m; }
static long long elem(void) { long long a[2]; a[1] = m16(); return a[1]; }
static long long compound(void) { long long d = 0; d += m16(); return d; }
static long long compound_mul(void) { long long d = 3; d *= m16(); return d; }
static long long ternary(int f) { long long d = f ? m16() : (long long)5; return d; }
static long long ret_int(void) { int x = m16(); return x; }
static long long from_unsigned(void) { long long d = u16(); return d; }
static long long from_char(void) { long long d = c16(); return d; }
static int cmp_neg(void) { long long d = m16(); return (d < 0) * 10 + (d == -16); }

int main(void)
{
    static unsigned char buf[64];
    static int ibuf[16];
    unsigned char *a = pp(buf + 24);
    unsigned char *b = pp(buf + 8);
    show64("pdiff", pdiff(a, b));
    printf("pdiff_shr %d\n", pdiff_shr(a, b));
    show64("idiff", idiff(ip(ibuf + 9), ip(ibuf + 2)));
    printf("in_range %d%d%d%d\n", in_range(a, a + 4000), in_range(a + 4000, a),
           in_range(a, a + (1 << 28)), in_range(a + (1 << 28), a));
    printf("trunc %d %d %d %d\n", trunc_cmp(4000), trunc_cmp(-16), trunc_val(-16), trunc_val(4000));
    show64("init_const", init_const());
    show64("init_call", init_call());
    show64("init_param", init_param(-16));
    show64("assign", assign());
    show64("global", global());
    show64("member", member());
    show64("elem", elem());
    show64("compound", compound());
    show64("compound_mul", compound_mul());
    show64("ternary", ternary(1));
    show64("ternary0", ternary(0));
    show64("ret_int", ret_int());
    show64("from_unsigned", from_unsigned());
    show64("from_char", from_char());
    printf("cmp_neg %d\n", cmp_neg());
    return (int)(init_const() >> 32) & 0xFF;
}
