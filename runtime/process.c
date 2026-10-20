/* The process calls, for a machine that runs one program at a time.
 *
 * The program is process 1 and has no parent.  It cannot make another
 * process -- s32_execv runs a program and waits for it, which is not
 * fork's contract or exec's -- so fork and the exec family fail with
 * ENOSYS, and wait and waitpid, with no child to report, fail with
 * ECHILD.  sysconf and the resource limits say what the machine imposes
 * (<unistd.h>, <sys/resource.h>).  Nothing here touches the machine, so
 * both libc archives carry it. */
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include <sys/resource.h>
#include <sys/wait.h>

/* docs/SPEC.md: the host's table of guest descriptors */
#define GUEST_FDS 128

pid_t getpid(void) { return 1; }
pid_t getppid(void) { return 0; }

pid_t fork(void) {
    errno = ENOSYS;
    return -1;
}

int execv(const char *path, char *const argv[]) {
    (void)path; (void)argv;
    errno = ENOSYS;
    return -1;
}

int execve(const char *path, char *const argv[], char *const envp[]) {
    (void)path; (void)argv; (void)envp;
    errno = ENOSYS;
    return -1;
}

int execvp(const char *file, char *const argv[]) {
    (void)file; (void)argv;
    errno = ENOSYS;
    return -1;
}

pid_t wait(int *status) {
    (void)status;
    errno = ECHILD;
    return -1;
}

pid_t waitpid(pid_t pid, int *status, int options) {
    (void)pid; (void)status;
    if (options & ~(WNOHANG | WUNTRACED)) {
        errno = EINVAL;
        return -1;
    }
    errno = ECHILD;
    return -1;
}

long sysconf(int name) {
    switch (name) {
    case _SC_OPEN_MAX:         return GUEST_FDS;
    case _SC_PAGESIZE:         return 4096;
    case _SC_NPROCESSORS_CONF:
    case _SC_NPROCESSORS_ONLN: return 1;
    default:
        errno = EINVAL;
        return -1;
    }
}

int getdtablesize(void) { return GUEST_FDS; }

static int limit_of(int resource, struct rlimit *r) {
    switch (resource) {
    case RLIMIT_NOFILE:
        r->rlim_cur = r->rlim_max = GUEST_FDS;
        return 0;
    case RLIMIT_CORE:
        r->rlim_cur = r->rlim_max = 0;
        return 0;
    case RLIMIT_CPU: case RLIMIT_FSIZE: case RLIMIT_DATA:
    case RLIMIT_STACK: case RLIMIT_AS:
        r->rlim_cur = r->rlim_max = RLIM_INFINITY;
        return 0;
    default:
        return -1;
    }
}

int getrlimit(int resource, struct rlimit *rlim) {
    if (limit_of(resource, rlim) < 0) {
        errno = EINVAL;
        return -1;
    }
    return 0;
}

/* Nothing enforces a limit set here, so the only setting that is true
 * is the one already in force: past the hard limit is EPERM (as for an
 * unprivileged process), and anything else short of it EINVAL. */
int setrlimit(int resource, const struct rlimit *rlim) {
    struct rlimit now;
    if (limit_of(resource, &now) < 0 || rlim->rlim_cur > rlim->rlim_max) {
        errno = EINVAL;
        return -1;
    }
    if (rlim->rlim_max > now.rlim_max) {
        errno = EPERM;
        return -1;
    }
    if (rlim->rlim_cur != now.rlim_cur || rlim->rlim_max != now.rlim_max) {
        errno = EINVAL;
        return -1;
    }
    return 0;
}

int getrusage(int who, struct rusage *usage) {
    if (who != RUSAGE_SELF && who != RUSAGE_CHILDREN) {
        errno = EINVAL;
        return -1;
    }
    char *p = (char *)usage;
    for (unsigned i = 0; i < sizeof *usage; i++) p[i] = 0;
    /* no processor time to report (libc_debug, or a host that cannot say)
     * is zero, and not this call's failure: errno is left as it was */
    int saved = errno;
    clock_t c;
    if (who == RUSAGE_SELF && (c = clock()) != (clock_t)-1) {
        usage->ru_utime.tv_sec = (time_t)(c / CLOCKS_PER_SEC);
        usage->ru_utime.tv_usec = (suseconds_t)(c % CLOCKS_PER_SEC);
    }
    errno = saved;
    return 0;
}
