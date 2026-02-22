/* Phase 6 test: #define, #include, #ifdef/#ifndef/#else/#endif */

int write(int fd, char *buf, int len);

int my_strlen(char *s) {
    int n;
    n = 0;
    while (*(s + n) != 0) {
        n = n + 1;
    }
    return n;
}

int my_puts(char *s) {
    int len;
    len = my_strlen(s);
    write(1, s, len);
    return 0;
}

/* --- #define tests --- */

#define VAL_TEN 10
#define VAL_TWENTY 20
#define HEX_FF 0xFF
#define NEG_FIVE -5

/* --- #include test --- */

#include "test_phase6_inc.h"

/* --- #ifdef / #ifndef / #else / #endif tests --- */

#define FEATURE_A 1

#ifdef FEATURE_A
int has_feature_a(void) { return 1; }
#else
int has_feature_a(void) { return 0; }
#endif

#ifdef FEATURE_B
int has_feature_b(void) { return 1; }
#else
int has_feature_b(void) { return 0; }
#endif

#ifndef FEATURE_B
int missing_feature_b(void) { return 1; }
#else
int missing_feature_b(void) { return 0; }
#endif

#ifndef FEATURE_A
int missing_feature_a(void) { return 1; }
#else
int missing_feature_a(void) { return 0; }
#endif

/* --- #if 0 / #if 1 tests --- */

#if 0
int if_zero_func(void) { return 99; }
#endif

#if 1
int if_one_func(void) { return 1; }
#endif

/* --- #if defined() tests --- */

#define HAS_THING 1

#if defined(HAS_THING)
int check_defined(void) { return 1; }
#else
int check_defined(void) { return 0; }
#endif

#if defined(NO_SUCH_THING)
int check_not_defined(void) { return 1; }
#else
int check_not_defined(void) { return 0; }
#endif

/* --- #if !defined() test --- */

#if !defined(NO_SUCH_THING)
int check_not_defined2(void) { return 1; }
#else
int check_not_defined2(void) { return 0; }
#endif

/* --- #if with comparison --- */

#define MY_VERSION 5

#if MY_VERSION > 3
int version_ok(void) { return 1; }
#else
int version_ok(void) { return 0; }
#endif

/* --- #if with && --- */

#define OPT_A 1
#define OPT_B 1

#if defined(OPT_A) && defined(OPT_B)
int both_opts(void) { return 1; }
#else
int both_opts(void) { return 0; }
#endif

#if defined(OPT_A) && defined(NO_SUCH_THING)
int one_missing(void) { return 1; }
#else
int one_missing(void) { return 0; }
#endif

/* --- #if with || --- */

#if defined(OPT_A) || defined(NO_SUCH_THING)
int either_opt(void) { return 1; }
#else
int either_opt(void) { return 0; }
#endif

#if defined(NO_SUCH_THING) || defined(NO_SUCH_THING2)
int neither_opt(void) { return 1; }
#else
int neither_opt(void) { return 0; }
#endif

/* --- #if / #elif / #else / #endif chain --- */

#define PICK 2

#if PICK == 1
int pick_val(void) { return 10; }
#elif PICK == 2
int pick_val(void) { return 20; }
#elif PICK == 3
int pick_val(void) { return 30; }
#else
int pick_val(void) { return 0; }
#endif

/* elif where first branch taken */
#if PICK == 2
int pick_first(void) { return 100; }
#elif PICK == 2
int pick_first(void) { return 200; }
#endif

/* elif falls through to else */
#define PICK_NONE 99

#if PICK_NONE == 1
int pick_else(void) { return 10; }
#elif PICK_NONE == 2
int pick_else(void) { return 20; }
#else
int pick_else(void) { return 30; }
#endif

/* --- #undef tests --- */

#define TEMP_DEF 42

int before_undef(void) { return TEMP_DEF; }

#undef TEMP_DEF

#ifdef TEMP_DEF
int after_undef(void) { return 99; }
#else
int after_undef(void) { return 0; }
#endif

/* re-define after undef */
#define TEMP_DEF 77

int redef_val(void) { return TEMP_DEF; }

#undef TEMP_DEF

/* --- #if arithmetic --- */

#define AX 10
#define AY 20

#if (AX + AY) == 30
int arith_ok(void) { return 1; }
#else
int arith_ok(void) { return 0; }
#endif

/* --- Nested #if inside #ifdef --- */

#define OUTER 1
#define INNER 1

#ifdef OUTER
#if defined(INNER)
int nested_both(void) { return 1; }
#else
int nested_both(void) { return 0; }
#endif
#endif

/* --- #ifdef / #elif chain --- */

#define FEAT_X 1

#ifdef FEAT_X
int ifdef_elif(void) { return 10; }
#elif defined(FEAT_Y)
int ifdef_elif(void) { return 20; }
#else
int ifdef_elif(void) { return 30; }
#endif

/* --- #line skip test (simulating Ragel output) --- */
#line 100 "fake_file.c"

/* --- Test: basic define values --- */
int test_define_basic(void) {
    if (VAL_TEN != 10) return 1;
    if (VAL_TWENTY != 20) return 2;
    return 0;
}

/* --- Test: hex define --- */
int test_define_hex(void) {
    if (HEX_FF != 255) return 1;
    return 0;
}

/* --- Test: negative define --- */
int test_define_neg(void) {
    if (NEG_FIVE != -5) return 1;
    return 0;
}

/* --- Test: define in expression --- */
int test_define_expr(void) {
    int x;
    x = VAL_TEN + VAL_TWENTY;
    if (x != 30) return 1;
    x = VAL_TEN * 3;
    if (x != 30) return 2;
    return 0;
}

/* --- Test: #include brought in functions --- */
int test_include(void) {
    int r;
    r = included_add(3, 4);
    if (r != 7) return 1;
    r = included_mul(5, 6);
    if (r != 30) return 2;
    return 0;
}

/* --- Test: #ifdef / #ifndef --- */
int test_ifdef(void) {
    if (has_feature_a() != 1) return 1;
    if (has_feature_b() != 0) return 2;
    if (missing_feature_b() != 1) return 3;
    if (missing_feature_a() != 0) return 4;
    return 0;
}

/* --- Test: #if 0 / #if 1 --- */
int test_if_basic(void) {
    /* if_zero_func should not exist; if_one_func should return 1 */
    if (if_one_func() != 1) return 1;
    return 0;
}

/* --- Test: #if defined() --- */
int test_if_defined(void) {
    if (check_defined() != 1) return 1;
    if (check_not_defined() != 0) return 2;
    if (check_not_defined2() != 1) return 3;
    return 0;
}

/* --- Test: #if comparison --- */
int test_if_compare(void) {
    if (version_ok() != 1) return 1;
    return 0;
}

/* --- Test: #if && / || --- */
int test_if_logical(void) {
    if (both_opts() != 1) return 1;
    if (one_missing() != 0) return 2;
    if (either_opt() != 1) return 3;
    if (neither_opt() != 0) return 4;
    return 0;
}

/* --- Test: #if / #elif / #else chain --- */
int test_elif(void) {
    if (pick_val() != 20) return 1;
    if (pick_first() != 100) return 2;
    if (pick_else() != 30) return 3;
    return 0;
}

/* --- Test: #undef --- */
int test_undef(void) {
    if (before_undef() != 42) return 1;
    if (after_undef() != 0) return 2;
    if (redef_val() != 77) return 3;
    return 0;
}

/* --- Test: #if arithmetic --- */
int test_if_arith(void) {
    if (arith_ok() != 1) return 1;
    return 0;
}

/* --- Test: nested #if inside #ifdef --- */
int test_nested_if(void) {
    if (nested_both() != 1) return 1;
    return 0;
}

/* --- Test: #ifdef with #elif --- */
int test_ifdef_elif(void) {
    if (ifdef_elif() != 10) return 1;
    return 0;
}

/* --- Test: enum still works alongside #define --- */
enum { ENUM_X = 42, ENUM_Y = 100 };

int test_enum_regression(void) {
    if (ENUM_X != 42) return 1;
    if (ENUM_Y != 100) return 2;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_define_basic();
    if (rc) return rc;

    rc = test_define_hex();
    if (rc) return rc + 10;

    rc = test_define_neg();
    if (rc) return rc + 20;

    rc = test_define_expr();
    if (rc) return rc + 30;

    rc = test_include();
    if (rc) return rc + 40;

    rc = test_ifdef();
    if (rc) return rc + 50;

    rc = test_enum_regression();
    if (rc) return rc + 60;

    rc = test_if_basic();
    if (rc) return rc + 70;

    rc = test_if_defined();
    if (rc) return rc + 80;

    rc = test_if_compare();
    if (rc) return rc + 90;

    rc = test_if_logical();
    if (rc) return rc + 100;

    rc = test_elif();
    if (rc) return rc + 110;

    rc = test_undef();
    if (rc) return rc + 120;

    rc = test_if_arith();
    if (rc) return rc + 130;

    rc = test_nested_if();
    if (rc) return rc + 140;

    rc = test_ifdef_elif();
    if (rc) return rc + 150;

    my_puts("Phase 6: all tests passed\n");
    return 0;
}
