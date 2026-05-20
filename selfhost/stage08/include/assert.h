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
#define assert(cond) ((cond) ? (void)0 : abort())
#endif

#endif
