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
#define EPERM   1
#define ENOENT  2
#define EIO     5
#define EBADF   9
#define EACCES 13
#define EEXIST 17
#define ENOTDIR 20
#define EISDIR 21
#define ENOSYS 38

#endif
