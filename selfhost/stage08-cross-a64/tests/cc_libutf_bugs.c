/* Six stage08 defects surfaced by running libutf's 503-test harness on
 * SLOW-32 (2026-09-03).  Each block returns its own bit so a failure
 * names itself in the exit code; 0 means all six hold. */
typedef unsigned long long u64;

/* 1. sizeof a block-scope array whose size is inferred from its
 *    initializer was rounded up to the frame slot's multiple of 4. */
static int t_local_sizeof(void) {
    const unsigned char a5[] = { 0xC3, 0xA9, 0xE4, 0xB8, 0x96 };
    unsigned char a9[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    struct { char c[5]; } s5;
    struct { char c[7]; } s7 = { { 1, 2, 3, 4, 5, 6, 7 } };
    if (sizeof a5 != 5 || sizeof a9 != 9) return 1;
    if (sizeof s5 != 5 || sizeof s7 != 7) return 1;
    return a5[4] == 0x96 && a9[8] == 9 && s7.c[6] == 7 ? 0 : 1;
}

/* 2. a #define whose body is one literal was folded to a 32-bit int,
 *    so a 64-bit literal came back as its sign-extended low word. */
#define FP_INIT 1469598103934665603ULL
#define BIG_HEX 0xCBF29CE484222325ULL
#define TEN_DIGITS 4294967296ULL
#define NEG_LL -5000000000LL
static int t_wide_define(void) {
    u64 a = FP_INIT;
    u64 b = BIG_HEX;
    u64 c = TEN_DIGITS;
    long long d = NEG_LL;
    if ((unsigned)(a >> 32) != 0x14650FB0u || (unsigned)a != 0x739D0383u) return 2;
    if ((unsigned)(b >> 32) != 0xCBF29CE4u || (unsigned)b != 0x84222325u) return 2;
    if ((unsigned)(c >> 32) != 1u || (unsigned)c != 0u) return 2;
    if (d != -5000000000LL) return 2;
    return 0;
}

/* 3. store-to-load forwarding keyed by address VALUE ignored every
 *    store through a different address value, so aliasing stores were
 *    invisible: same-typed pointers, a union's narrow store under a
 *    word, a byte store inside a word, a struct member stored after a
 *    compound-literal init and returned by value. */
typedef struct { short fg, bg; unsigned char r, g, b, x; } CS;
#define CS_NORMAL ((CS){ -1, -1, 0, 0, 0, 0 })
static CS mk_bg(int idx) { CS cs = CS_NORMAL; cs.bg = (short)idx; return cs; }
static int two_ptrs(int *p, int *q) { *p = 10; *q = 20; return *p; }
static int short_in_word(void) { union { int w; short h[2]; } u; u.w = -1; u.h[1] = 5; return u.w; }
static int char_in_word(int *p) { *p = -1; ((unsigned char *)p)[1] = 0x42; return *p; }
static int distinct_ok(int *p) { int a = 1; *p = 2; return a; }   /* must still forward */
static int t_alias(void) {
    int x = 0;
    CS cs = mk_bg(5);
    if (cs.bg != 5 || cs.fg != -1) return 4;
    if (two_ptrs(&x, &x) != 20) return 4;
    if (short_in_word() != 0x0005FFFF) return 4;
    if ((unsigned)char_in_word(&x) != 0xFFFF42FFu) return 4;
    if (distinct_ok(&x) != 1) return 4;
    return 0;
}

/* 4. _Static_assert's message may be several adjacent literals
 *    (Ragel/table generators write "a" "b" "c"); parse error before. */
_Static_assert(sizeof(int) == 4, "int" " is " "four bytes");
static int t_static_assert(void) {
    _Static_assert(sizeof(short) == 2, "short" " is two");
    return 0;
}

/* 5. identical string literals are pooled: one .rodata copy, one
 *    address (as clang and gcc do; a libutf test compares them). */
static const char *lit_a(void) { return "same text"; }
static const char *lit_b(void) { return "same" " text"; }
static int t_string_pool(void) {
    const char *p = "same text";
    if (p != lit_a() || p != lit_b()) return 16;
    if (p == "other text") return 16;
    return 0;
}

int main(void) {
    int rc = 0;
    rc |= t_local_sizeof();
    rc |= t_wide_define();
    rc |= t_alias();
    rc |= t_static_assert();
    rc |= t_string_pool();
    return rc;                      /* the runner judges the exit code */
}
