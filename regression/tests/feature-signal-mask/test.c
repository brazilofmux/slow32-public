/* The signal mask, sigaction and kill, as a program alone sees them.
 * Every line is what glibc and macOS print for the same program: a
 * blocked signal waits and arrives once, before the unblocking call
 * returns, and several such run last-set-up first; a handler runs with its signal blocked
 * unless SA_NODEFER, and with sa_mask besides; SA_RESETHAND, SA_SIGINFO;
 * ignoring a pending signal discards it; SIGKILL cannot be blocked. */
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

static char trail[64];
static int depth, deepest, calls;

static void note(char c) {
    size_t n = strlen(trail);
    if (n + 1 < sizeof trail) { trail[n] = c; trail[n + 1] = '\0'; }
}

static void on_usr1(int sig) { (void)sig; note('1'); }
static void on_usr2(int sig) { (void)sig; note('2'); }
static void on_hup(int sig) { (void)sig; note('h'); }

/* raises its own signal the first three times it is called */
static void again(int sig) {
    depth++;
    if (depth > deepest) deepest = depth;
    note('[');
    if (++calls <= 3) raise(sig);
    note(']');
    depth--;
}

static void masker(int sig) {
    (void)sig;
    note('<');
    raise(SIGUSR2);
    note('>');
}

static int info_signo = -1;
static void with_info(int sig, siginfo_t *si, void *ctx) {
    (void)sig; (void)ctx;
    info_signo = si->si_signo;
}

static void fresh(void) { trail[0] = '\0'; }

static void install(int sig, void (*fn)(int), int flags, int masked) {
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = fn;
    sigemptyset(&sa.sa_mask);
    if (masked) sigaddset(&sa.sa_mask, masked);
    sa.sa_flags = flags;
    sigaction(sig, &sa, NULL);
}

int main(void) {
    sigset_t s, old, pend;

    install(SIGUSR1, on_usr1, 0, 0);
    install(SIGUSR2, on_usr2, 0, 0);
    install(SIGHUP, on_hup, 0, 0);

    fresh();
    sigemptyset(&s);
    sigaddset(&s, SIGUSR1);
    sigprocmask(SIG_BLOCK, &s, &old);
    raise(SIGUSR1);
    raise(SIGUSR1);
    sigpending(&pend);
    printf("blocked: ran \"%s\", pending %d\n", trail, sigismember(&pend, SIGUSR1));
    sigprocmask(SIG_SETMASK, &old, NULL);
    printf("unblocked: ran \"%s\" (once)\n", trail);
    sigpending(&pend);
    printf("pending after: %d\n", sigismember(&pend, SIGUSR1));

    fresh();
    sigemptyset(&s);
    sigaddset(&s, SIGUSR1);
    sigaddset(&s, SIGUSR2);
    sigaddset(&s, SIGHUP);
    sigprocmask(SIG_BLOCK, &s, NULL);
    raise(SIGUSR2);
    raise(SIGHUP);
    raise(SIGUSR1);
    sigprocmask(SIG_UNBLOCK, &s, NULL);
    printf("three pending, delivered in order \"%s\"\n", trail);

    fresh();
    depth = deepest = calls = 0;
    install(SIGUSR1, again, 0, 0);
    raise(SIGUSR1);
    printf("handler raising itself: \"%s\" deepest %d\n", trail, deepest);

    fresh();
    depth = deepest = calls = 0;
    install(SIGUSR1, again, SA_NODEFER, 0);
    raise(SIGUSR1);
    printf("same, SA_NODEFER: \"%s\" deepest %d\n", trail, deepest);

    fresh();
    install(SIGUSR1, masker, 0, SIGUSR2);
    raise(SIGUSR1);
    printf("sa_mask holds SIGUSR2 off: \"%s\"\n", trail);
    fresh();
    install(SIGUSR1, masker, 0, 0);
    raise(SIGUSR1);
    printf("without it: \"%s\"\n", trail);

    fresh();
    install(SIGUSR1, on_usr1, SA_RESETHAND, 0);
    raise(SIGUSR1);
    struct sigaction now;
    sigaction(SIGUSR1, NULL, &now);
    printf("SA_RESETHAND: ran \"%s\", now %s\n", trail, now.sa_handler == SIG_DFL ? "SIG_DFL" : "still set");

    struct sigaction si;
    memset(&si, 0, sizeof si);
    si.sa_sigaction = with_info;
    si.sa_flags = SA_SIGINFO;
    sigemptyset(&si.sa_mask);
    sigaction(SIGUSR2, &si, NULL);
    raise(SIGUSR2);
    printf("SA_SIGINFO: si_signo is SIGUSR2 %d\n", info_signo == SIGUSR2);

    fresh();
    install(SIGUSR1, on_usr1, 0, 0);
    sigemptyset(&s);
    sigaddset(&s, SIGUSR1);
    sigprocmask(SIG_BLOCK, &s, NULL);
    raise(SIGUSR1);
    signal(SIGUSR1, SIG_IGN);
    sigpending(&pend);
    printf("ignoring a pending signal discards it: %d\n", sigismember(&pend, SIGUSR1) == 0);
    signal(SIGUSR1, on_usr1);
    sigprocmask(SIG_UNBLOCK, &s, NULL);
    printf("and nothing arrives later: \"%s\"\n", trail);

    sigfillset(&s);
    sigprocmask(SIG_BLOCK, &s, &old);
    sigprocmask(SIG_SETMASK, &old, &s);
    printf("fill blocks SIGKILL %d, SIGSTOP %d, SIGTERM %d\n", sigismember(&s, SIGKILL),
           sigismember(&s, SIGSTOP), sigismember(&s, SIGTERM));

    struct sigaction sk;
    memset(&sk, 0, sizeof sk);
    sk.sa_handler = on_usr1;
    int r;
    errno = 0;
    r = sigaction(SIGKILL, &sk, NULL);
    printf("sigaction(SIGKILL): %d EINVAL %d\n", r, errno == EINVAL);
    errno = 0;
    r = sigprocmask(12345, &s, NULL);
    printf("sigprocmask bad how: %d EINVAL %d\n", r, errno == EINVAL);

    fresh();
    printf("kill self: %d", kill(getpid(), SIGUSR1));
    printf(" ran \"%s\"\n", trail);
    printf("kill(self, 0): %d\n", kill(getpid(), 0));
    errno = 0;
    r = kill(0x7ffffff0, 0);
    printf("kill(absent, 0): %d ESRCH %d\n", r, errno == ESRCH);
    return 0;
}
