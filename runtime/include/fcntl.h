#ifndef SLOW32_FCNTL_H
#define SLOW32_FCNTL_H

#ifdef __cplusplus
extern "C" {
#endif

#define O_RDONLY 0x01
#define O_WRONLY 0x02
#define O_RDWR   0x03
#define O_APPEND 0x04
#define O_CREAT  0x08
#define O_TRUNC  0x10

#define O_ACCMODE 0x03

/* Accepted and ignored by open: a file here never blocks, and nothing
 * inherits a descriptor across s32_execv.  open() strips them before the
 * machine sees the flags (it refuses bits it does not know).  fcntl's
 * F_SETFL does set O_NONBLOCK, which matters on a socket. */
#define O_NONBLOCK 0x0800
#define O_CLOEXEC  0x80000

/* fcntl: these four (Linux's numbers).  F_GETFL gives the access mode,
 * O_APPEND and O_NONBLOCK; F_SETFL changes O_APPEND and O_NONBLOCK, but
 * not on fds 0-2 while they are still the emulator's own standard streams
 * (EPERM).  FD_CLOEXEC is remembered; nothing is inherited either way.
 * docs/SPEC.md 8.6, FCNTL. */
#define F_GETFD 1
#define F_SETFD 2
#define F_GETFL 3
#define F_SETFL 4
#define FD_CLOEXEC 1

int open(const char *pathname, int flags, ...);
int fcntl(int fd, int cmd, ...);

#ifdef __cplusplus
}
#endif
#endif
