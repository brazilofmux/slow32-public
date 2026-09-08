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
/* GitHub issue 39: a comment in an #if expression is whitespace.
 * Without that, 1 then a block comment then && 0 yields 1. */
#if 1 /*c*/ && 0
#define IF_COMMENT_TRUE 1
#endif
#define IFC_A 1
#define IFC_B 1
#if defined(IFC_A) /*x*/ && defined(IFC_B)
#define IFC_BOTH 1
#endif
#if 1 // not division
#define IF_SLASHSLASH 1
#endif
static int t_if_line_mark = __LINE__;
#if 1 \
    && 1
#endif
static int t_if_line_after = __LINE__;
#define PP_BIG 1000000000
#if PP_BIG == 1000000000
#define PP_BIG_OK 1
#endif
#define PP_SUM 1+2
#if PP_SUM * 3 == 9
#define PP_SUM_OK 1
#endif
#define PP_INNER 1+2
#define PP_OUTER PP_INNER * 3
#if PP_OUTER == 9
#define PP_NEST_OK 1
#endif
static int t_if_continuation(void) {
#ifndef CONT_SELECTED
    return 4;
#endif
#ifdef CONT_WRONG
    return 4;
#endif
#ifdef IF_COMMENT_TRUE
    return 4;
#endif
#ifndef IFC_BOTH
    return 4;
#endif
#ifndef IF_SLASHSLASH
    return 4;
#endif
    /* mark, #if, continued, #endif, after: 4 lines.  A splice that
     * does not bump lex_line makes after-mark 3. */
    if (t_if_line_after - t_if_line_mark != 4) return 4;
#ifndef PP_BIG_OK
    return 4;
#endif
#ifndef PP_SUM_OK
    return 4;
#endif
#ifndef PP_NEST_OK
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

/* 9. A stray `;` at file scope (SQLite's shell: SQLITE_EXTENSION_INIT1; with
 *    the macro empty). */
;
#define EMPTY_INIT
EMPTY_INIT;

/* 8. A hex/octal literal with bit 31 set is unsigned int; as a signed
 *    int it sign-extended into the high word of a 64-bit flags field.
 *    Unsuffixed decimal 2147483648 is long long, not unsigned int
 *    (C11 6.4.4.1 / GitHub issue 40): `2147483648 > -1` is true
 *    signed and false if the same bits are unsigned. */
typedef unsigned long long u64;
static int t_big_literal(void) {
    u64 f = 0;
    unsigned int u = 0xFFFFFFFF;
    u64 g;
    f |= 0x80000000;
    if ((unsigned)(f >> 32) != 0) return 128;
    g = 0x80000000;
    if ((unsigned)(g >> 32) != 0 || (unsigned)g != 0x80000000) return 128;
    g = u;
    if ((unsigned)(g >> 32) != 0) return 128;
    if (!(0x80000000 > 5) || !(0xFFFFFFFF > 0)) return 128;
    if ((f & 0xFFFFFFFF80000000ULL) != 0x80000000ULL) return 128;
    if (!(2147483648 > -1)) return 128;
    if (0x80000000 > -1) return 128;
    if (020000000000 > -1) return 128;
    return 0;
}


/* 10. ~ and ! in constant expressions (shell.c: static long ctrlMask = ~0L) */
static long t10_mask = ~0L;
static int t10_c = ~0;
static unsigned t10_u = ~0u;
static int t10_n = !0;
static int t10_z = !5;
static long long t10_ll = ~0LL;
static int t10_hi = !(1LL << 32);
static int t10_hi2 = !(0x100000000LL);
static int t10_case(int v) {
    switch (v) {
    case ~1: return 1;
    case !0 + 1: return 2;
    }
    return 0;
}
static int t_const_unary(void) {
    int ok = 1;
    if (t10_mask != -1L) ok = 0;
    if (t10_c != -1) ok = 0;
    if (t10_u != 0xFFFFFFFFu) ok = 0;
    if (t10_n != 1 || t10_z != 0) ok = 0;
    if (t10_ll != -1LL) ok = 0;
    if (t10_hi != 0) ok = 0;
    if (t10_hi2 != 0) ok = 0;
    if (t10_case(-2) != 1 || t10_case(2) != 2 || t10_case(3) != 0) ok = 0;
    return ok ? 0 : 512;
}


/* 11. grouping parens around a file-scope declarator (shell.c: char *(azHelp[])) */
static const char *(t11_help[]) = { "alpha", "beta", "gamma" };
static int (t11_n) = 3;
static int t_grouped_declarator(void) {
    int ok = 1;
    if (t11_n != 3) ok = 0;
    if (sizeof(t11_help) / sizeof(t11_help[0]) != 3) ok = 0;
    if (t11_help[2][0] != 'g') ok = 0;
    return ok ? 0 : 1024;
}


/* 12. (*p->m)(args) through a function-pointer member (OP_Function's
 * (*pCtx->pFunc->xSFunc)(pCtx, argc, argv)); the member is pointer-typed,
 * so the "strip the no-op star" rule missed it and the call went through
 * the code word the pointer named. */
struct t12_F { int nArg; void (*xSFunc)(int, int); };
struct t12_C { struct t12_F *pFunc; int argc; };
static int t12_got;
static void t12_fn(int a, int b) { t12_got = a * 100 + b; }
static struct t12_F t12_f = { 2, t12_fn };
static struct t12_C t12_c = { &t12_f, 7 };
static int t_fnptr_member_deref(void) {
    struct t12_C *p = &t12_c;
    void (*g)(int, int) = t12_fn;
    int ok = 1;
    (*p->pFunc->xSFunc)(1, 2); if (t12_got != 102) ok = 0;
    (p->pFunc->xSFunc)(3, 4);  if (t12_got != 304) ok = 0;
    p->pFunc->xSFunc(5, 6);    if (t12_got != 506) ok = 0;
    (*g)(7, 8);                if (t12_got != 708) ok = 0;
    (*t12_c.pFunc->xSFunc)(9, 1); if (t12_got != 901) ok = 0;
    return ok ? 0 : 2048;
}

int main(void) {
    int r = 0;
    r |= t_big_literal();
    r |= t_unsigned_cmp_imm(5);
    r |= t_tentative();
    r |= t_if_continuation();
    r |= t_local_static_init();
    r |= t_stringize_lines();
    r |= t_reexpand(13);
    r |= t_frontend_forms();
    r |= t_const_unary();
    r |= t_grouped_declarator();
    r |= t_fnptr_member_deref();
    return r;
}
