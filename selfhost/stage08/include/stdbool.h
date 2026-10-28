/* stdbool.h -- C11 7.18: bool is a macro for the built-in _Bool.
 *
 * It was `typedef unsigned char bool`, which made a bool a byte and a
 * conversion to it a truncation -- bool b = 256 held 0 -- where _Bool
 * itself converts to 0 or 1 (6.3.1.2).  Same size and layout as before. */
#ifndef _STDBOOL_H
#define _STDBOOL_H
#define bool _Bool
#define true 1
#define false 0
#define __bool_true_false_are_defined 1
#endif
