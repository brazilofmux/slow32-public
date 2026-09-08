/* assert.h -- s12cc-compatible stub
 *
 * Defines `assert(cond)` that hits abort() on false.  Static-assert
 * support is via the C11 `_Static_assert` keyword if used at all.
 */
#ifndef _ASSERT_H
#define _ASSERT_H

void abort(void);

#ifdef NDEBUG
#define assert(cond) ((void)0)
#else
void __assert_fail(const char *expr, const char *file, int line);
/* Says which assertion, and where (the line is the preprocessor's count,
 * which runs ahead of the source in a big file -- the expression text is
 * what to grep for). */
#define assert(cond) ((cond) ? (void)0 : __assert_fail(#cond, __FILE__, __LINE__))
#endif

#endif
