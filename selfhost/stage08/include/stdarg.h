/* stdarg.h -- s12cc-compatible stub.
 *
 * cc-x64 / cc-a64 / s12cc recognize `va_start` / `va_arg` / `va_end` as
 * parser intrinsics; this header only has to provide the va_list type
 * so source code that `#include <stdarg.h>` parses cleanly.
 *
 * va_list is intentionally `char *` (not a struct).  On the cross
 * backends it holds a pointer to a frame control block (GP + FP
 * cursors + shared stack overflow); on slow32 native it is a
 * frame-pointer cookie.  Either representation fits in one pointer. */
#ifndef _STDARG_H
#define _STDARG_H

typedef char *va_list;

#endif
