/* signal, raise, sigaction, the signal mask, and kill.
 *
 * Nothing outside the program sends it a signal on SLOW-32, so raise --
 * and abort and kill, through it -- is the only way a handler runs.  A
 * program that installs a handler and raises the signal gets what the
 * standard says: the handler is called with the signal's number before
 * raise returns, an ignored signal is ignored, and the default action ends
 * the run with the status a shell reports for a process a signal killed
 * (128 + the number).  A handler stays installed after it runs (BSD's
 * rule, and glibc's) unless sigaction asked for SA_RESETHAND.
 *
 * The mask is POSIX's.  A blocked signal that is raised is pending until
 * it is unblocked, and is delivered before the sigprocmask (or the end of
 * the handler) that unblocks it returns; a signal is pending once, however
 * often it was raised.  A handler runs with its own signal blocked, unless
 * SA_NODEFER, and with sa_mask blocked besides; signal() installs one as
 * sigaction with SA_RESTART would.  Setting a signal's action to ignore it
 * discards it if it is pending.
 *
 * The numbers are Linux's.  SIGKILL and SIGSTOP cannot be caught, ignored
 * or blocked; SIGCHLD, SIGCONT, SIGURG and SIGWINCH are ignored by default.
 * kill reaches the program itself (its pid, 0, or -1) and nothing else:
 * the machine runs one program.
 *
 * The self-hosted library has its own signal and raise (selfhost/stage08/
 * libc/posix_proc.c); regression/libc-tests/signal_raise.c holds the two,
 * and the host's, to one behaviour.
 */
#include <signal.h>
#include <errno.h>
#include <unistd.h>

/* exit_mmio.c / exit_debug.c: what was written is sent, the run ends
 * with 128 + sig, and no atexit function runs */
extern void __s32_killed(int sig);

static struct sigaction actions[NSIG];          /* zero is SIG_DFL */
static sigset_t blocked, pending;

#define BIT(sig) (1ul << (sig))
#define UNBLOCKABLE (BIT(SIGKILL) | BIT(SIGSTOP))
#define ALL_SIGNALS (~0ul & ~1ul)               /* 1..31; bit 0 is no signal */

static int valid(int sig) { return sig >= 1 && sig < NSIG; }

static int ignored_by_default(int sig) {
    return sig == SIGCHLD || sig == SIGCONT || sig == SIGURG || sig == SIGWINCH;
}

static int ignores(int sig, const struct sigaction *a) {
    if (a->sa_flags & SA_SIGINFO) return 0;
    return a->sa_handler == SIG_IGN || (a->sa_handler == SIG_DFL && ignored_by_default(sig));
}

/* Deliver every pending signal the mask lets through, as a kernel does
 * on the way back to the program: it takes the lowest such signal, sets up
 * its handler's frame -- the handler's mask now in force -- and looks
 * again, so each frame lands on top of the last and the last set up runs
 * first (glibc and macOS both run SIGHUP, SIGUSR1, SIGUSR2, pending
 * together and unblocked together, as SIGUSR2, SIGUSR1, SIGHUP).  Here the
 * recursion is the frame set-up, at most one level per pending signal;
 * what a handler leaves pending is taken by the loop around it, so a
 * handler that raises its own signal is called again and again without
 * the stack growing. */
static void deliver_pending(void) {
    sigset_t ready;
    while ((ready = pending & ~blocked) != 0) {
        int sig = __builtin_ctzl(ready);
        pending &= ~BIT(sig);
        struct sigaction a = actions[sig];
        if (ignores(sig, &a)) continue;
        if (!(a.sa_flags & SA_SIGINFO) && a.sa_handler == SIG_DFL) __s32_killed(sig);

        sigset_t saved = blocked;
        blocked |= a.sa_mask;
        if (!(a.sa_flags & SA_NODEFER)) blocked |= BIT(sig);
        blocked &= ~UNBLOCKABLE;
        if (a.sa_flags & SA_RESETHAND) {
            actions[sig].sa_handler = SIG_DFL;
            actions[sig].sa_flags = 0;
            actions[sig].sa_mask = 0;
        }
        deliver_pending();                      /* the frames above this one */
        if (a.sa_flags & SA_SIGINFO) {
            siginfo_t info = { 0 };
            info.si_signo = sig;
            info.si_code = SI_USER;
            info.si_pid = getpid();
            a.sa_sigaction(sig, &info, (void *)0);
        } else {
            a.sa_handler(sig);
        }
        blocked = saved;
    }
}

int sigaction(int sig, const struct sigaction *act, struct sigaction *oldact) {
    if (!valid(sig) || (act && (sig == SIGKILL || sig == SIGSTOP))) {
        errno = EINVAL;
        return -1;
    }
    if (oldact) *oldact = actions[sig];
    if (act) {
        actions[sig] = *act;
        actions[sig].sa_mask &= ALL_SIGNALS & ~UNBLOCKABLE;
        if (ignores(sig, &actions[sig])) pending &= ~BIT(sig);
    }
    return 0;
}

sig_handler_t signal(int sig, sig_handler_t handler) {
    struct sigaction act, old;
    act.sa_handler = handler;
    act.sa_mask = 0;
    act.sa_flags = SA_RESTART;
    if (sigaction(sig, &act, &old) < 0) return SIG_ERR;
    return old.sa_handler;
}

int raise(int sig) {
    if (!valid(sig)) {
        errno = EINVAL;
        return -1;
    }
    pending |= BIT(sig);
    deliver_pending();
    return 0;
}

int kill(pid_t pid, int sig) {
    pid_t self = getpid();
    if (sig != 0 && !valid(sig)) {
        errno = EINVAL;
        return -1;
    }
    if (pid != self && pid != 0 && pid != -1 && pid != -self) {
        errno = ESRCH;
        return -1;
    }
    return sig == 0 ? 0 : raise(sig);
}

int sigprocmask(int how, const sigset_t *set, sigset_t *oldset) {
    sigset_t old = blocked;
    if (set) {
        sigset_t s = *set & ALL_SIGNALS & ~UNBLOCKABLE;
        if (how == SIG_BLOCK) blocked |= s;
        else if (how == SIG_UNBLOCK) blocked &= ~s;
        else if (how == SIG_SETMASK) blocked = s;
        else {
            errno = EINVAL;
            return -1;
        }
    }
    if (oldset) *oldset = old;
    deliver_pending();
    return 0;
}

int sigpending(sigset_t *set) {
    *set = pending;
    return 0;
}

int sigemptyset(sigset_t *set) {
    *set = 0;
    return 0;
}

int sigfillset(sigset_t *set) {
    *set = ALL_SIGNALS;
    return 0;
}

int sigaddset(sigset_t *set, int sig) {
    if (!valid(sig)) {
        errno = EINVAL;
        return -1;
    }
    *set |= BIT(sig);
    return 0;
}

int sigdelset(sigset_t *set, int sig) {
    if (!valid(sig)) {
        errno = EINVAL;
        return -1;
    }
    *set &= ~BIT(sig);
    return 0;
}

int sigismember(const sigset_t *set, int sig) {
    if (!valid(sig)) {
        errno = EINVAL;
        return -1;
    }
    return (*set & BIT(sig)) != 0;
}
