#ifndef _SIGNAL_H
#define _SIGNAL_H

#ifdef __cplusplus
extern "C" {
#endif

typedef int sig_atomic_t;
typedef void (*sig_handler_t)(int);
typedef sig_handler_t sighandler_t;

#define SIG_DFL ((sig_handler_t)0)
#define SIG_IGN ((sig_handler_t)1)
#define SIG_ERR ((sig_handler_t)-1)

/* Linux's numbers */
#define SIGINT   2
#define SIGILL   4
#define SIGABRT  6
#define SIGBUS   7
#define SIGFPE   8
#define SIGKILL  9
#define SIGSEGV  11
#define SIGALRM  14
#define SIGTERM  15

/* Nothing outside the program sends it a signal on SLOW-32: a handler
 * runs when the program raises the signal itself (raise, abort).  See
 * signal.c. */
sig_handler_t signal(int sig, sig_handler_t handler);
int raise(int sig);

#ifdef __cplusplus
}
#endif

#endif
