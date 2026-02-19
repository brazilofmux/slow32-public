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

int puts(char *s) {
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

    puts("Phase 6: all tests passed\n");
    return 0;
}
