// runtime/include/wchar.h (SLOW-32)
// Types only.  The library has no wide-character functions; this header
// exists for mbstate_t and the limits, which C++'s <string> and <cwchar>
// need even with wide characters turned off.
#ifndef _SLOW32_WCHAR_H_
#define _SLOW32_WCHAR_H_

#include <stddef.h>
#include <stdint.h>

typedef unsigned int wint_t;   /* clang's __WINT_TYPE__; WINT_MIN/MAX in <stdint.h> */
typedef struct { unsigned int __count; unsigned int __value; } mbstate_t;

#define WEOF ((wint_t)-1)

#endif
