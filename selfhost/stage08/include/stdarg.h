/* stdarg.h -- s12cc-compatible stub.
 *
 * cc-x64 / cc-a64 / s12cc recognize `va_start` / `va_arg` / `va_end` as
 * parser intrinsics; this header only has to provide the va_list type
 * so source code that `#include <stdarg.h>` parses cleanly.
 *
 * va_list is intentionally `char *` (not a struct) because the
 * intrinsics consume it as an opaque tagged pointer on the cross
 * backends and as a frame-pointer cookie on slow32 native; either
 * representation fits in 8 bytes (4 on slow32), so a single pointer
 * slot is enough. */
#ifndef _STDARG_H
#define _STDARG_H

typedef char *va_list;

#endif
