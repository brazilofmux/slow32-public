/* signal_stubs.c — minimal AArch64 Linux syscall wrappers for the
 * signal/timer/clock APIs that tools/dbt uses.
 *
 * The simple ones (_exit, sysconf, mmap, munmap, mprotect, kill) are real
 * syscalls here.  sigaction, signal, setitimer, getitimer, clock_gettime
 * and getpid moved to ../cross-include/libc_linux.c (shared with libc_x64)
 * and are real too: dbt's fault handler and -Q probe timer work.
 */

#include <unistd.h>    /* getpid, in ../cross-include/libc_linux.c */

long __syscall();
int clock_gettime(int clk, void *ts);   /* ../cross-include/libc_linux.c */

/* ---- Process control ---- */

void _exit(int code) {
    __syscall(94, code, 0, 0, 0, 0, 0);  /* exit_group */
}

/* getpid: ../cross-include/libc_linux.c */

int kill(int pid, int signo) {
    return __syscall(129, pid, signo, 0, 0, 0, 0);
}

int raise(int signo) {
    return kill(getpid(), signo);
}

/* ---- Time ---- */

/* clock_gettime: ../cross-include/libc_linux.c */

/* ---- Memory ---- */

char *mmap(char *addr, long length, int prot, int flags, int fd, long offset) {
    return (char *)__syscall(222, addr, length, prot, flags, fd, offset);
}

int munmap(char *addr, long length) {
    return __syscall(215, addr, length, 0, 0, 0, 0);
}

int mprotect(char *addr, long len, int prot) {
    return __syscall(226, addr, len, prot, 0, 0, 0);
}

/* ---- sysconf ---- */

#define _SC_PAGESIZE 30

long sysconf(int name) {
    if (name == _SC_PAGESIZE) return 4096;
    return -1;
}

/* ---- Signal stubs ---- */

/* dbt uses these for SIGSEGV/SIGBUS/SIGILL fault dumps, SIGINT, and
 * SIGALRM-driven probe sampling. Stubbed so dbt links and runs; the
 * diagnostics simply won't fire. Real rt_sigaction wrapping has a
 * tricky kernel struct layout (sa_restorer must point at a code
 * trampoline that issues syscall 139=rt_sigreturn) — defer until
 * the diagnostics are actually needed on a64. */
int sigemptyset(void *set) {
    if (set) *(long *)set = 0;
    return 0;
}

int sigfillset(void *set) {
    if (set) *(long *)set = -1;
    return 0;
}

int sigaddset(void *set, int signo) {
    if (set && signo > 0 && signo < 64) *(long *)set |= (1L << (signo - 1));
    return 0;
}

/* sigaction, signal, setitimer, getitimer: ../cross-include/libc_linux.c */

int gettimeofday(void *tv, void *tz) {
    /* Use clock_gettime(REALTIME) and split into tv_sec / tv_usec. */
    long ts[2];
    int rc = clock_gettime(0 /* CLOCK_REALTIME */, ts);
    if (rc < 0) return rc;
    if (tv) {
        ((long *)tv)[0] = ts[0];
        ((long *)tv)[1] = ts[1] / 1000;
    }
    return 0;
}

/* ---- Misc unistd stubs ---- */

/* isatty, unlink: ../cross-include/libc_linux.c */
