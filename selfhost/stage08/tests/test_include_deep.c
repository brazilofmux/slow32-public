/* #include nested 40 deep, past the 16 levels the include stack held
 * (pp.h, TAB_PP_INC): past them __LINE__ ran on from the including
 * file.  The file includes itself; the deepest level records its
 * own __LINE__.  Generated. */
#if !defined INC1
#define INC1
#include "test_include_deep.c"
#elif !defined INC2
#define INC2
#include "test_include_deep.c"
#elif !defined INC3
#define INC3
#include "test_include_deep.c"
#elif !defined INC4
#define INC4
#include "test_include_deep.c"
#elif !defined INC5
#define INC5
#include "test_include_deep.c"
#elif !defined INC6
#define INC6
#include "test_include_deep.c"
#elif !defined INC7
#define INC7
#include "test_include_deep.c"
#elif !defined INC8
#define INC8
#include "test_include_deep.c"
#elif !defined INC9
#define INC9
#include "test_include_deep.c"
#elif !defined INC10
#define INC10
#include "test_include_deep.c"
#elif !defined INC11
#define INC11
#include "test_include_deep.c"
#elif !defined INC12
#define INC12
#include "test_include_deep.c"
#elif !defined INC13
#define INC13
#include "test_include_deep.c"
#elif !defined INC14
#define INC14
#include "test_include_deep.c"
#elif !defined INC15
#define INC15
#include "test_include_deep.c"
#elif !defined INC16
#define INC16
#include "test_include_deep.c"
#elif !defined INC17
#define INC17
#include "test_include_deep.c"
#elif !defined INC18
#define INC18
#include "test_include_deep.c"
#elif !defined INC19
#define INC19
#include "test_include_deep.c"
#elif !defined INC20
#define INC20
#include "test_include_deep.c"
#elif !defined INC21
#define INC21
#include "test_include_deep.c"
#elif !defined INC22
#define INC22
#include "test_include_deep.c"
#elif !defined INC23
#define INC23
#include "test_include_deep.c"
#elif !defined INC24
#define INC24
#include "test_include_deep.c"
#elif !defined INC25
#define INC25
#include "test_include_deep.c"
#elif !defined INC26
#define INC26
#include "test_include_deep.c"
#elif !defined INC27
#define INC27
#include "test_include_deep.c"
#elif !defined INC28
#define INC28
#include "test_include_deep.c"
#elif !defined INC29
#define INC29
#include "test_include_deep.c"
#elif !defined INC30
#define INC30
#include "test_include_deep.c"
#elif !defined INC31
#define INC31
#include "test_include_deep.c"
#elif !defined INC32
#define INC32
#include "test_include_deep.c"
#elif !defined INC33
#define INC33
#include "test_include_deep.c"
#elif !defined INC34
#define INC34
#include "test_include_deep.c"
#elif !defined INC35
#define INC35
#include "test_include_deep.c"
#elif !defined INC36
#define INC36
#include "test_include_deep.c"
#elif !defined INC37
#define INC37
#include "test_include_deep.c"
#elif !defined INC38
#define INC38
#include "test_include_deep.c"
#elif !defined INC39
#define INC39
#include "test_include_deep.c"
#elif !defined INC40
#define INC40
static int deep_line = __LINE__;
#endif
#ifndef TEST_MAIN
#define TEST_MAIN
int main(void) {
    if (deep_line != 124) return 1;
    if (__LINE__ != 130) return 2;
    return 0;
}
#endif
