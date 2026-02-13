#ifndef _SIGNAL_H
#define _SIGNAL_H

#ifdef __cplusplus
extern "C" {
#endif

typedef int sig_atomic_t;
typedef void (*sig_handler_t)(int);

#define SIG_DFL ((sig_handler_t)0)
#define SIG_IGN ((sig_handler_t)1)
#define SIG_ERR ((sig_handler_t)-1)

#define SIGINT   2
#define SIGILL   4
#define SIGABRT  6
#define SIGFPE   8
#define SIGSEGV  11
#define SIGTERM  15

/* signal() - stub: always returns SIG_DFL (no signal support on bare metal) */
static inline sig_handler_t signal(int sig, sig_handler_t handler) {
    (void)sig;
    (void)handler;
    return SIG_DFL;
}

#ifdef __cplusplus
}
#endif

#endif
