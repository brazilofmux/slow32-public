/* fcntl.h -- open flags, as the MMIO host takes them
 * (common/mmio_ring_layout.h), and open itself.  (A comment here said
 * unistd.h declared open; nothing did, and every call went through the
 * implicit declaration -- harmless by accident, since open returns int
 * and the host ignores the mode.  Found by a syntax-only pass over the
 * kit's users with -Werror=implicit-function-declaration.) */
int open(const char *path, int flags, ...);
#ifndef _FCNTL_H
#define _FCNTL_H

#define O_RDONLY 0x01
#define O_WRONLY 0x02
#define O_RDWR   0x03
#define O_APPEND 0x04
#define O_CREAT  0x08
#define O_TRUNC  0x10

#endif
