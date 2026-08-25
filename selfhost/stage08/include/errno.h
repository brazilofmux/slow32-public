/* errno.h -- minimal errno for the stage08 libc.
 *
 * Added for David Gay's dtoa.c (strtod sets ERANGE); the values match
 * the runtime libc's so mixed links agree. */
#ifndef _ERRNO_H
#define _ERRNO_H

extern int errno;

#define EDOM   33
#define ERANGE 34
#define EINVAL 22
#define ENOMEM 12

#endif
