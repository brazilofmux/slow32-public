/* ucontext.h -- s12cc-compatible stub
 *
 * dbt.c only dereferences ucontext_t inside an `#if defined(__x86_64__)`
 * block; on AArch64 we never read its fields. Make the type opaque
 * enough to compile (size matches Linux mcontext for safety).
 */
#ifndef _UCONTEXT_H
#define _UCONTEXT_H

typedef struct {
    char _mcontext_blob[4096];   /* opaque — never accessed on a64 */
} mcontext_t;

typedef struct ucontext {
    unsigned long     uc_flags;
    struct ucontext  *uc_link;
    char              uc_stack[24];
    mcontext_t        uc_mcontext;
    char              uc_sigmask[128];
} ucontext_t;

#endif
