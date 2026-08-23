#ifndef _STRINGS_H
#define _STRINGS_H

/* BSD string header: the historical home of the case-insensitive
 * compares. Doom was the first tenant to ask for it. */

#include <stddef.h>

int strcasecmp(const char *s1, const char *s2);
int strncasecmp(const char *s1, const char *s2, size_t n);

#endif /* _STRINGS_H */
