/* assert.h -- C11 7.2, for both SLOW-32 C libraries (runtime/include and
 * selfhost/stage08/include hold the same file).
 *
 * No include guard around assert: each inclusion defines it by NDEBUG as
 * it stands there (7.2p1).  A failing assert writes the expression, the
 * function, the file and the line to stderr and calls abort (7.2.1.1p2;
 * runtime/assert.c). */
#undef assert
#ifdef NDEBUG
#define assert(ignore) ((void)0)
#else
#ifdef __cplusplus
extern "C" __attribute__((noreturn))
#else
_Noreturn
#endif
void __s32_assert_func(const char *expr, const char *file, int line, const char *func);
#define assert(expr) \
    ((expr) ? (void)0 : __s32_assert_func(#expr, __FILE__, __LINE__, __func__))
#endif

#ifndef _ASSERT_H
#define _ASSERT_H
#ifndef __cplusplus
#define static_assert _Static_assert
#endif
#endif
