/* syscalls.c — POSIX I/O wrappers via Linux syscalls */

#include <sys/resource.h>

int __syscall();

int write(int fd, char *buf, int len) {
    return __syscall(1, fd, buf, len, 0, 0, 0);
}

int read(int fd, char *buf, int len) {
    return __syscall(0, fd, buf, len, 0, 0, 0);
}

int open(char *path, int flags, int mode) {
    return __syscall(2, path, flags, mode, 0, 0, 0);
}

int close(int fd) {
    return __syscall(3, fd, 0, 0, 0, 0, 0);
}

int lseek(int fd, int offset, int whence) {
    return __syscall(8, fd, offset, whence, 0, 0, 0);
}

int stat(char *path, char *buf) {
    return __syscall(4, path, buf, 0, 0, 0, 0);
}

int fstat(int fd, char *buf) {
    return __syscall(5, fd, buf, 0, 0, 0, 0);
}

int sys_brk(int addr) {
    return __syscall(12, addr, 0, 0, 0, 0, 0);
}

void exit(int code) {
    __syscall(60, code, 0, 0, 0, 0, 0);
}

/* RLIMIT_STACK and friends.  struct rlimit is two unsigned longs (cur,
 * max) per the Linux x86-64 ABI for syscalls 97 / 160.  No prlimit64
 * wrapper — legacy setrlimit (160) is enough for dbt-x64's needs and
 * matches the kernel's struct layout exactly. */

int getrlimit(int resource, struct rlimit *rlim) {
    return __syscall(97, resource, rlim, 0, 0, 0, 0);
}

int setrlimit(int resource, struct rlimit *rlim) {
    return __syscall(160, resource, rlim, 0, 0, 0, 0);
}
