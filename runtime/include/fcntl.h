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

/* Accepted and ignored: a file here never blocks, and nothing inherits a
 * descriptor across s32_execv.  open() strips them before the machine
 * sees the flags (it refuses bits it does not know). */
#define O_NONBLOCK 0x0800
#define O_CLOEXEC  0x80000

int open(const char *pathname, int flags, ...);

#ifdef __cplusplus
}
#endif
#endif
