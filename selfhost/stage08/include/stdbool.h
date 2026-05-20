/* stdbool.h -- C99 _Bool / C++ bool support.
 *
 * The parser-level builtin already registers `bool` and `_Bool` as
 * `unsigned char`, but a translation unit that *only* gets bool from
 * including this header (e.g. early-bootstrap stages) needs the same
 * 1-byte typedef to keep struct layouts agreeing with gcc/clang. */
#ifndef _STDBOOL_H
#define _STDBOOL_H

typedef unsigned char bool;
#define true 1
#define false 0

#endif
