/* signal and raise.
 *
 * Nothing outside the program sends it a signal on SLOW-32, so raise --
 * and abort, through it -- is the only way a handler runs.  A program
 * that installs a handler and raises the signal gets what the standard
 * says: the handler is called with the signal's number before raise
 * returns, an ignored signal is ignored, and the default action ends
 * the run with the status a shell reports for a process a signal killed
 * (128 + the number).  A handler stays installed after it runs (BSD's
 * rule, and glibc's).
 *
 * The numbers are Linux's.  SIGKILL and SIGSTOP cannot be caught;
 * SIGCHLD, SIGCONT, SIGURG and SIGWINCH are ignored by default.
 *
 * The self-hosted library has its own (selfhost/stage08/libc/
 * posix_proc.c); regression/libc-tests/signal_raise.c holds the two,
 * and the host's, to one behaviour.
 */
#include <signal.h>
#include <errno.h>

#define S32_NSIG 32

/* exit_mmio.c / exit_debug.c: what was written is sent, the run ends
 * with 128 + sig, and no atexit function runs */
extern void __s32_killed(int sig);

static sig_handler_t handlers[S32_NSIG];        /* 0 is SIG_DFL */

sig_handler_t signal(int sig, sig_handler_t handler) {
    if (sig < 1 || sig >= S32_NSIG || sig == 9 || sig == 19) {
        errno = EINVAL;
        return SIG_ERR;
    }
    sig_handler_t prev = handlers[sig];
    handlers[sig] = handler;
    return prev;
}

int raise(int sig) {
    if (sig < 1 || sig >= S32_NSIG) {
        errno = EINVAL;
        return -1;
    }
    sig_handler_t handler = handlers[sig];
    if (handler == SIG_IGN) return 0;
    if (handler == SIG_DFL) {
        if (sig == 17 || sig == 18 || sig == 23 || sig == 28) return 0;
        __s32_killed(sig);
    }
    handler(sig);
    return 0;
}
