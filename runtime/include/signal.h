#ifndef _SIGNAL_H
#define _SIGNAL_H

#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef int sig_atomic_t;
typedef void (*sig_handler_t)(int);
typedef sig_handler_t sighandler_t;

#define SIG_DFL ((sig_handler_t)0)
#define SIG_IGN ((sig_handler_t)1)
#define SIG_ERR ((sig_handler_t)-1)

/* Linux's numbers.  NSIG is one past the largest. */
#define SIGHUP    1
#define SIGINT    2
#define SIGQUIT   3
#define SIGILL    4
#define SIGTRAP   5
#define SIGABRT   6
#define SIGIOT    SIGABRT
#define SIGBUS    7
#define SIGFPE    8
#define SIGKILL   9
#define SIGUSR1   10
#define SIGSEGV   11
#define SIGUSR2   12
#define SIGPIPE   13
#define SIGALRM   14
#define SIGTERM   15
#define SIGSTKFLT 16
#define SIGCHLD   17
#define SIGCONT   18
#define SIGSTOP   19
#define SIGTSTP   20
#define SIGTTIN   21
#define SIGTTOU   22
#define SIGURG    23
#define SIGXCPU   24
#define SIGXFSZ   25
#define SIGVTALRM 26
#define SIGPROF   27
#define SIGWINCH  28
#define SIGIO     29
#define SIGPOLL   SIGIO
#define SIGPWR    30
#define SIGSYS    31
#define NSIG      32

/* Nothing outside the program sends it a signal on SLOW-32: a handler
 * runs when the program raises the signal itself (raise, abort).  See
 * signal.c. */
sig_handler_t signal(int sig, sig_handler_t handler);
int raise(int sig);

/* Signal sets, the mask, and sigaction.  A blocked signal that is raised
 * waits, pending, until it is unblocked; it is delivered before the
 * sigprocmask that unblocks it returns.  A handler runs with its own
 * signal blocked (unless SA_NODEFER) and sa_mask besides; signal()
 * installs a handler as sigaction with SA_RESTART does.  kill reaches
 * only the program itself -- the machine runs one (signal.c). */
typedef unsigned long sigset_t;

int sigemptyset(sigset_t *set);
int sigfillset(sigset_t *set);
int sigaddset(sigset_t *set, int sig);
int sigdelset(sigset_t *set, int sig);
int sigismember(const sigset_t *set, int sig);

#define SIG_BLOCK   0
#define SIG_UNBLOCK 1
#define SIG_SETMASK 2
int sigprocmask(int how, const sigset_t *set, sigset_t *oldset);
int sigpending(sigset_t *set);

union sigval {
    int sival_int;
    void *sival_ptr;
};

typedef struct {
    int si_signo;
    int si_errno;
    int si_code;
    pid_t si_pid;
    uid_t si_uid;
    int si_status;
    void *si_addr;
    union sigval si_value;
} siginfo_t;

#define SI_USER 0

struct sigaction {
    union {
        sig_handler_t __sa_handler;
        void (*__sa_sigaction)(int, siginfo_t *, void *);
    } __sa_u;
    sigset_t sa_mask;
    int sa_flags;
};
#define sa_handler   __sa_u.__sa_handler
#define sa_sigaction __sa_u.__sa_sigaction

#define SA_NOCLDSTOP 0x00000001
#define SA_NOCLDWAIT 0x00000002
#define SA_SIGINFO   0x00000004
#define SA_ONSTACK   0x08000000
#define SA_RESTART   0x10000000
#define SA_NODEFER   0x40000000
#define SA_RESETHAND 0x80000000

int sigaction(int sig, const struct sigaction *act, struct sigaction *oldact);

int kill(pid_t pid, int sig);

#ifdef __cplusplus
}
#endif

#endif
