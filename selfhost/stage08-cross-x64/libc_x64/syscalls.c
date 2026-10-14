/* syscalls.c — POSIX I/O wrappers via Linux syscalls */

#include <sys/resource.h>

int __syscall();

/* write, read, open, close, lseek, stat and fstat are in
 * ../cross-include/libc_linux.c now, with errno and the kernel's struct
 * stat, shared with libc_a64. */

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
