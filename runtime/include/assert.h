#ifndef _ASSERT_H
#define _ASSERT_H

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif
/* Prints "Assertion failed: EXPR, file FILE, line LINE" and aborts
 * (stdlib_extra.c).  Called through a declaration: the macro once
 * expanded to printf(), which needed <stdio.h> where assert was used. */
__attribute__((noreturn)) void __s32_assert_fail(const char *expr, const char *file, int line);
#ifdef __cplusplus
}
#endif

#ifdef NDEBUG
#define assert(expr) ((void)0)
#else
#define assert(expr) \
    ((expr) ? (void)0 : __s32_assert_fail(#expr, __FILE__, __LINE__))
#endif

#endif /* _ASSERT_H */
