/* signal_stubs.c — minimal AArch64 Linux syscall wrappers for the
 * signal/timer/clock APIs that tools/dbt uses.
 *
 * Strategy: implement the simple ones (clock_gettime, _exit, sysconf,
 * mmap, munmap, mprotect, getpid, kill) via real syscalls. Stub out
 * the more complex sigaction/signal/setitimer paths as no-ops for
 * now — dbt's signal-based diagnostics won't fire, but normal
 * dispatch-loop operation works.
 */

int __syscall();

/* ---- Process control ---- */

void _exit(int code) {
    __syscall(94, code, 0, 0, 0, 0, 0);  /* exit_group */
}

int getpid(void) {
    return __syscall(172, 0, 0, 0, 0, 0, 0);
}

int kill(int pid, int signo) {
    return __syscall(129, pid, signo, 0, 0, 0, 0);
}

int raise(int signo) {
    return kill(getpid(), signo);
}

/* ---- Time ---- */

/* `struct timespec { long tv_sec; long tv_nsec; }` — kernel layout for
 * clock_gettime is two 64-bit fields, matching `long` on AArch64. */
int clock_gettime(int clk, void *ts) {
    return __syscall(113, clk, ts, 0, 0, 0, 0);
}

/* ---- Memory ---- */

char *mmap(char *addr, long length, int prot, int flags, int fd, long offset) {
    return (char *)(long)__syscall(222, addr, length, prot, flags, fd, offset);
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

int sigaction(int signo, void *act, void *oldact) {
    return 0;  /* no-op: signal handlers do not fire */
}

void *signal(int signo, void *handler) {
    return 0;  /* no-op: returns SIG_DFL-equivalent (NULL) */
}

int setitimer(int which, void *new, void *old) {
    return 0;  /* no-op: probe timer disabled */
}

int getitimer(int which, void *cur) {
    return 0;
}

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

int isatty(int fd) {
    return 0;  /* conservative: pretend nothing is a tty */
}

int unlink(char *path) {
    /* unlinkat(AT_FDCWD, path, 0) */
    return __syscall(35, -100, path, 0, 0, 0, 0);
}
