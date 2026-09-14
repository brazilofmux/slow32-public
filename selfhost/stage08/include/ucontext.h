/* ucontext.h -- s12cc-compatible ucontext_t with the Linux register-file
 * layout for the two hosts the cross-compilers target.
 *
 * tools/dbt/dbt.c's fault handler reads uc_mcontext.gregs[REG_RIP] /
 * gregs[REG_RSP] on x86-64 and uc_mcontext.pc / .sp on AArch64 to say
 * where the host faulted (ba2b9125).  An earlier version of this stub
 * made mcontext_t an opaque blob on the theory that AArch64 never read
 * it, and `make dbt` in both cross trees stopped compiling the day that
 * theory stopped holding (GitHub issue 82).  The layouts below are
 * glibc's, verified by offsetof() against the real headers:
 *
 *   x86-64:  uc_mcontext at 40, gregs at 40, REG_RSP 15, REG_RIP 16,
 *            sizeof(mcontext_t) 256, uc_sigmask at 296, total 968.
 *   AArch64: uc_sigmask at 40, uc_mcontext at 176 (16-aligned), sp at
 *            +256, pc at +264, pstate at +272, __reserved at +288,
 *            sizeof(mcontext_t) 4384, total 4560.
 *
 * The selfhost libc's sigaction() is a no-op today (signal_stubs.c /
 * extras.c), so the handler is never entered from a self-hosted build;
 * this only has to be right, not exercised.  s12cc gives a struct the
 * alignment of its widest member (8), so the 16-byte alignment the
 * kernel gives uc_mcontext and __reserved on AArch64 is spelled out as
 * explicit padding.
 */
#ifndef _UCONTEXT_H
#define _UCONTEXT_H

#if defined(__x86_64__)

#define REG_R8     0
#define REG_R9     1
#define REG_R10    2
#define REG_R11    3
#define REG_R12    4
#define REG_R13    5
#define REG_R14    6
#define REG_R15    7
#define REG_RDI    8
#define REG_RSI    9
#define REG_RBP   10
#define REG_RBX   11
#define REG_RDX   12
#define REG_RAX   13
#define REG_RCX   14
#define REG_RSP   15
#define REG_RIP   16
#define REG_EFL   17
#define REG_CSGSFS 18
#define REG_ERR   19
#define REG_TRAPNO 20
#define REG_OLDMASK 21
#define REG_CR2   22

typedef struct {
    unsigned long   gregs[23];        /* 0 .. 184 */
    void           *fpregs;           /* 184 */
    unsigned long   __reserved1[8];   /* 192 .. 256 */
} mcontext_t;

typedef struct ucontext {
    unsigned long     uc_flags;       /* 0 */
    struct ucontext  *uc_link;        /* 8 */
    char              uc_stack[24];   /* 16: stack_t */
    mcontext_t        uc_mcontext;    /* 40 */
    char              uc_sigmask[128];/* 296 */
    char              __fpregs_mem[512];
    unsigned long     __ssp[4];
} ucontext_t;                         /* 968 */

#else /* AArch64 Linux */

typedef struct {
    unsigned long   fault_address;    /* 0 */
    unsigned long   regs[31];         /* 8 .. 256 */
    unsigned long   sp;               /* 256 */
    unsigned long   pc;               /* 264 */
    unsigned long   pstate;           /* 272 */
    unsigned long   __pad0;           /* 280: __reserved is 16-aligned */
    unsigned char   __reserved[4096]; /* 288 .. 4384 */
} mcontext_t;

typedef struct ucontext {
    unsigned long     uc_flags;       /* 0 */
    struct ucontext  *uc_link;        /* 8 */
    char              uc_stack[24];   /* 16: stack_t */
    char              uc_sigmask[128];/* 40: sigset_t + glibc's __unused, 1024 bits */
    unsigned long     __pad0;         /* 168: uc_mcontext is 16-aligned */
    mcontext_t        uc_mcontext;    /* 176 */
} ucontext_t;                         /* 4560 */

#endif

#endif
