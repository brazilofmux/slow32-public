/* stage08 defects and gaps surfaced by compiling SQLite 3.51.0's
 * amalgamation (2026-09-08).  Each block returns its own bit so a failure
 * names itself in the exit code; 0 means all hold.  The suite compiles
 * this file with -mlong-calls, so every direct call here also exercises
 * the address-forming call sequence. */

/* 1. sltiu zero-extends its immediate, so `x <u -1` folded to an
 *    immediate compared against 4095.  SQLite's 64-bit compares hit it. */
static int t_unsigned_cmp_imm(unsigned x) {
    unsigned all = (unsigned)-1;
    if (!(x < all)) return 1;              /* 5 <u 0xFFFFFFFF */
    if (!(x < (unsigned)-8)) return 1;     /* 5 <u 0xFFFFFFF8 */
    if (x > (unsigned)-1) return 1;
    if (!(all >= (unsigned)-8)) return 1;
    return 0;
}

/* 2. A tentative definition followed by the real one emitted the label
 *    twice (sqlite3WhereTrace). */
int tentative_v;
int tentative_v = 7;
static int t_tentative(void) { return tentative_v == 7 ? 0 : 2; }

/* 3. An #if expression stopped at the physical newline: SQLite's
 *    allocator selection spans four continued lines. */
#if defined(NOT_A) \
  + defined(NOT_B) \
  + defined(NOT_C)==0
#define CONT_SELECTED 1
#endif
#if !HAVE_LT_R && !HAVE_LT_S \
    && defined(NOT_D) && defined(NOT_E)
#define CONT_WRONG 1
#endif
static int t_if_continuation(void) {
#ifndef CONT_SELECTED
    return 4;
#endif
#ifdef CONT_WRONG
    return 4;
#endif
    return 0;
}

/* 4. A block-scope static pointer with a string initializer, and a
 *    block-scope static double (sqlite3VdbePrintOp's zFormat1). */
static int t_local_static_init(void) {
    static const char *zFormat1 = "%4d %-13s";
    static double half = 0.5;
    static int n = 3;
    if (zFormat1[1] != '4' || zFormat1[4] != '%') return 8;
    if (half + half != 1.0) return 8;
    if (n != 3) return 8;
    return 0;
}

/* 5. Stringizing an argument that spans lines left a raw newline in
 *    the string (SQLite's two-line asserts). */
#define STR(x) #x
static int t_stringize_lines(void) {
    const char *s = STR( a ? b
                         : c );
    const char *t = "a ? b : c";
    int i = 0;
    while (s[i] && t[i] && s[i] == t[i]) i++;
    return (s[i] == 0 && t[i] == 0) ? 0 : 16;
}

/* 6. A function-like macro used right after its own earlier expansion,
 *    inside another macro, was left unexpanded: the earlier expansion's
 *    disable entry lingered over the prefix a backwards expansion
 *    reused (ROUND8 inside assert, after ROUND8). */
#define ROUND8(x) (((x)+7)&~7)
#define CHECK(c) ((c) ? 0 : 32)
static int t_reexpand(int v) {
    int n = ROUND8(v);
    return CHECK( ROUND8(n)==n );
}

/* 7. Front-end forms the amalgamation uses: a function returning a
 *    function pointer, an array of function pointers at file scope, a
 *    nested function-pointer struct member, unary plus, a bare `signed`,
 *    qualifiers after the stars. */
static int inc(int x) { return x + 1; }
static int dbl(int x) { return x + x; }
static int (*getf(int which))(int) { return which ? dbl : inc; }
static int (*ftab[2])(int) = { inc, dbl };
struct vfs { void (*(*xDlSym)(void *, const char *))(void); int n; };
static void nop(void) {}
static void (*dlsym_stub(void *p, const char *z))(void) { (void)p; (void)z; return nop; }
static int t_frontend_forms(void) {
    struct vfs v;
    signed s = -2;
    int one = +1;
    char *const *const pp = 0;
    v.xDlSym = dlsym_stub;
    v.n = (signed)sizeof(int);
    if (getf(0)(1) != 2 || getf(1)(3) != 6) return 64;
    if (ftab[0](4) != 5 || ftab[1](4) != 8) return 64;
    if (v.xDlSym(0, "x") != nop || v.n != 4) return 64;
    if (s + one != -1 || pp != 0) return 64;
    return 0;
}

int main(void) {
    int r = 0;
    r |= t_unsigned_cmp_imm(5);
    r |= t_tentative();
    r |= t_if_continuation();
    r |= t_local_static_init();
    r |= t_stringize_lines();
    r |= t_reexpand(13);
    r |= t_frontend_forms();
    return r;
}
