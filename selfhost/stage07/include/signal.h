/* signal.h -- s12cc-compatible stub
 *
 * Just enough POSIX signal API for tools/dbt's diagnostics handlers
 * (SIGSEGV/SIGBUS/SIGILL fault dump, SIGINT, SIGALRM probe timer).
 * Implementation in libc_a64/signal_stubs.c — currently no-op stubs;
 * signals will not actually fire, which is fine for initial bring-up
 * (default kernel actions still terminate the process on faults).
 */
#ifndef _SIGNAL_H
#define _SIGNAL_H

#ifndef NULL
#define NULL 0
#endif

typedef int sig_atomic_t;
typedef unsigned long sigset_t;

/* Linux AArch64 signal numbers (same as x86 for these). */
#define SIGINT   2
#define SIGILL   4
#define SIGABRT  6
#define SIGBUS   7
#define SIGSEGV 11
#define SIGALRM 14
#define SIGTERM 15

/* sigaction flags (Linux ABI bits we care about). */
#define SA_SIGINFO 0x00000004

/* Function pointer type for signal handlers — declared up front so
   SIG_DFL/SIG_IGN can use a typedef cast (cc-a64's parser doesn't
   accept the full `(void (*)(int))0` form). */
typedef void (*sighandler_t)(int);

/* Special handler values. */
#define SIG_DFL ((sighandler_t)0)
#define SIG_IGN ((sighandler_t)1)

/* siginfo_t — only the fields dbt.c reads (via si_addr in the
   x86_64-only block we don't compile). Keep the struct big enough
   that dereferencing through it is safe. */
typedef struct {
    int   si_signo;
    int   si_errno;
    int   si_code;
    int   _pad0;
    void *si_addr;
    char  _pad1[112];   /* match Linux siginfo_t (~128 bytes) */
} siginfo_t;

struct sigaction {
    void (*sa_handler)(int);
    void (*sa_sigaction)(int, siginfo_t *, void *);
    sigset_t sa_mask;
    int      sa_flags;
    void   (*sa_restorer)(void);
};

int sigemptyset(sigset_t *set);
int sigfillset(sigset_t *set);
int sigaddset(sigset_t *set, int signo);
int sigaction(int signo, struct sigaction *act, struct sigaction *oldact);
sighandler_t signal(int signo, sighandler_t handler);
int kill(int pid, int signo);
int raise(int signo);

#endif
