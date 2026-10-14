/* fcntl.h -- Linux open flags and fcntl commands (cross builds). */
#ifndef _FCNTL_H
#define _FCNTL_H

#define O_RDONLY    00
#define O_WRONLY    01
#define O_RDWR      02
#define O_ACCMODE   03
#define O_CREAT     0100
#define O_EXCL      0200
#define O_NOCTTY    0400
#define O_TRUNC     01000
#define O_APPEND    02000
#define O_NONBLOCK  04000
#define O_CLOEXEC   02000000
#if defined(__aarch64__)
#define O_DIRECTORY 040000
#define O_NOFOLLOW  0100000
#else
#define O_DIRECTORY 0200000
#define O_NOFOLLOW  0400000
#endif

#define F_DUPFD     0
#define F_GETFD     1
#define F_SETFD     2
#define F_GETFL     3
#define F_SETFL     4
#define FD_CLOEXEC  1

#define AT_FDCWD            (-100)
#define AT_SYMLINK_NOFOLLOW 0x100
#define AT_REMOVEDIR        0x200

int open(const char *path, int flags, int mode);
int fcntl(int fd, int cmd, long arg);

#endif
