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

#include <sys/resource.h>

long __syscall();

/* write, read, open, close, lseek, stat and fstat are in
 * ../cross-include/libc_linux.c now, with errno and the kernel's struct
 * stat, shared with libc_x64. */

char *sys_brk(char *addr) {
    return (char *)__syscall(214, addr, 0, 0, 0, 0, 0);
}

void exit(int code) {
    __syscall(93, code, 0, 0, 0, 0, 0);
}

/* AArch64's asm-generic table has no legacy getrlimit/setrlimit — only
 * prlimit64 (261).  Wrap it so dbt.c's portable RLIMIT_STACK raise works.
 * struct rlimit on this target is two unsigned longs (cur, max) — the same
 * layout as the kernel's struct rlimit64 on 64-bit hosts, so we can pass
 * `rlim` straight through. */

int getrlimit(int resource, struct rlimit *rlim) {
    /* prlimit64(0, resource, NULL, rlim) */
    return __syscall(261, 0, resource, 0, rlim, 0, 0);
}

int setrlimit(int resource, struct rlimit *rlim) {
    /* prlimit64(0, resource, rlim, NULL) */
    return __syscall(261, 0, resource, rlim, 0, 0, 0);
}
