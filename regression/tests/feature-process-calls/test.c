/* The process calls on a machine that runs one program: it is process 1
 * with no parent, fork and exec fail with ENOSYS, wait finds no child,
 * sysconf and the resource limits report what the machine imposes (128
 * descriptors, no core files), setrlimit accepts only what is already in
 * force, and tmpnam names files that do not exist.  The status macros
 * decode Linux's encoding. */
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/wait.h>

static void says(const char *what, long r) {
    printf("%-26s %ld errno %s\n", what, r,
           errno == 0 ? "0" : errno == ENOSYS ? "ENOSYS" : errno == ECHILD ? "ECHILD" :
           errno == EINVAL ? "EINVAL" : errno == EPERM ? "EPERM" : "other");
}

int main(void) {
    char *const args[] = { "chaos", NULL };
    long r;

    printf("getpid %d getppid %d\n", (int)getpid(), (int)getppid());
    errno = 0; r = fork();                             says("fork", r);
    errno = 0; r = execv("/bin/nowhere", args);        says("execv", r);
    errno = 0; r = execvp("nowhere", args);            says("execvp", r);
    int st = 0x5a5a;
    errno = 0; r = wait(&st);                          says("wait", r);
    errno = 0; r = waitpid(-1, &st, WNOHANG);          says("waitpid WNOHANG", r);
    errno = 0; r = waitpid(-1, &st, 0x4000);           says("waitpid bad options", r);
    printf("status untouched %d\n", st == 0x5a5a);

    errno = 0; r = sysconf(_SC_OPEN_MAX);              says("sysconf OPEN_MAX", r);
    errno = 0; r = sysconf(_SC_PAGESIZE);              says("sysconf PAGESIZE", r);
    errno = 0; r = sysconf(_SC_NPROCESSORS_ONLN);      says("sysconf NPROCESSORS_ONLN", r);
    errno = 0; r = sysconf(9999);                      says("sysconf 9999", r);
    printf("getdtablesize %d\n", getdtablesize());

    struct rlimit rl, want;
    errno = 0; r = getrlimit(RLIMIT_NOFILE, &rl);      says("getrlimit NOFILE", r);
    printf("  cur %llu max %llu\n", (unsigned long long)rl.rlim_cur, (unsigned long long)rl.rlim_max);
    want = rl;
    errno = 0; r = setrlimit(RLIMIT_NOFILE, &want);    says("setrlimit NOFILE as is", r);
    want.rlim_max = want.rlim_cur = 4096;
    errno = 0; r = setrlimit(RLIMIT_NOFILE, &want);    says("setrlimit NOFILE 4096", r);
    want.rlim_cur = 64; want.rlim_max = 128;
    errno = 0; r = setrlimit(RLIMIT_NOFILE, &want);    says("setrlimit NOFILE 64", r);
    errno = 0; r = getrlimit(RLIMIT_CORE, &rl);        says("getrlimit CORE", r);
    printf("  cur %llu max %llu\n", (unsigned long long)rl.rlim_cur, (unsigned long long)rl.rlim_max);
    want.rlim_cur = want.rlim_max = RLIM_INFINITY;
    errno = 0; r = setrlimit(RLIMIT_CORE, &want);      says("setrlimit CORE unlimited", r);
    errno = 0; r = getrlimit(RLIMIT_STACK, &rl);       says("getrlimit STACK", r);
    printf("  unlimited %d\n", rl.rlim_cur == RLIM_INFINITY && rl.rlim_max == RLIM_INFINITY);
    errno = 0; r = getrlimit(77, &rl);                 says("getrlimit 77", r);

    struct rusage ru;
    memset(&ru, 0x5a, sizeof ru);
    errno = 0; r = getrusage(RUSAGE_SELF, &ru);        says("getrusage SELF", r);
    printf("  usec in range %d, stime 0 %d, maxrss 0 %d\n",
           ru.ru_utime.tv_usec >= 0 && ru.ru_utime.tv_usec < 1000000,
           ru.ru_stime.tv_sec == 0 && ru.ru_stime.tv_usec == 0, ru.ru_maxrss == 0);
    errno = 0; r = getrusage(42, &ru);                 says("getrusage 42", r);

    char a[L_tmpnam], b[L_tmpnam];
    char *pa = tmpnam(a), *pb = tmpnam(b);
    printf("tmpnam: own buffer %d, under P_tmpdir %d, distinct %d, absent %d\n",
           pa == a, strncmp(a, P_tmpdir "/", strlen(P_tmpdir) + 1) == 0,
           pb && strcmp(a, b) != 0, access(a, F_OK) != 0 && access(b, F_OK) != 0);
    char *ps = tmpnam(NULL);
    printf("tmpnam(NULL) static %d, fits L_tmpnam %d\n", ps != NULL && ps != a && ps != b,
           ps && strlen(ps) < L_tmpnam);

    tzset();
    struct timezone tz;
    printf("struct timezone %d bytes\n", (int)sizeof tz);

    printf("exit 3: exited %d status %d signaled %d\n", WIFEXITED(0x0300), WEXITSTATUS(0x0300),
           WIFSIGNALED(0x0300));
    printf("signal 9: exited %d signaled %d termsig %d\n", WIFEXITED(0x0009), WIFSIGNALED(0x0009),
           WTERMSIG(0x0009));
    printf("stopped 19: stopped %d stopsig %d signaled %d\n", WIFSTOPPED(0x137f), WSTOPSIG(0x137f),
           WIFSIGNALED(0x137f));
    return 0;
}
