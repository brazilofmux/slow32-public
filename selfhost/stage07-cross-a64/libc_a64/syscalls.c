/* syscalls.c — POSIX I/O wrappers via AArch64 Linux syscalls.
 *
 * The cc-a64 codegen recognises calls to __syscall(nr, a0, a1, a2, a3, a4, a5)
 * as a builtin and emits:
 *     mov x8, nr
 *     mov x0..x5, args
 *     svc #0
 * with the result left in x0 (which becomes the C return value).
 *
 * Linux AArch64 syscall numbers below are from arch/arm64/uapi/asm/unistd.h
 * (the asm-generic table) — different from x86-64.
 */

long __syscall();

int write(int fd, char *buf, int len) {
    return __syscall(64, fd, buf, len, 0, 0, 0);
}

int read(int fd, char *buf, int len) {
    return __syscall(63, fd, buf, len, 0, 0, 0);
}

/* AArch64's asm-generic does not have plain open/stat — they use the *at
 * variants with AT_FDCWD as fd. Wrap them so callers see a familiar API. */
#define AT_FDCWD (-100)

int open(char *path, int flags, int mode) {
    return __syscall(56, AT_FDCWD, path, flags, mode, 0, 0);  /* openat */
}

int close(int fd) {
    return __syscall(57, fd, 0, 0, 0, 0, 0);
}

int lseek(int fd, int offset, int whence) {
    return __syscall(62, fd, offset, whence, 0, 0, 0);
}

int stat(char *path, char *buf) {
    /* newfstatat(AT_FDCWD, path, buf, 0) */
    return __syscall(79, AT_FDCWD, path, buf, 0, 0, 0);
}

int fstat(int fd, char *buf) {
    return __syscall(80, fd, buf, 0, 0, 0, 0);
}

char *sys_brk(char *addr) {
    return (char *)__syscall(214, addr, 0, 0, 0, 0, 0);
}

void exit(int code) {
    __syscall(93, code, 0, 0, 0, 0, 0);
}
