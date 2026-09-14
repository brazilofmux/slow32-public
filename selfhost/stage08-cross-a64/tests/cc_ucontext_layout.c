/* The selfhost <ucontext.h> must match glibc's AArch64 layout: dbt.c's
 * fault handler reads uc_mcontext.pc / .sp from the kernel-provided
 * frame (GitHub issue 82).  Offsets measured with offsetof() against
 * the real header on Linux aarch64.  Exit 0 when every one agrees. */
#include <ucontext.h>
int main(void) {
    ucontext_t *u = 0;
    int bad = 0;
    if ((int)sizeof(ucontext_t) != 4560) bad |= 1;
    if ((int)(unsigned long)&u->uc_sigmask != 40) bad |= 2;
    if ((int)(unsigned long)&u->uc_mcontext != 176) bad |= 4;
    if ((int)sizeof(mcontext_t) != 4384) bad |= 8;
    if ((int)(unsigned long)&u->uc_mcontext.sp != 432) bad |= 16;
    if ((int)(unsigned long)&u->uc_mcontext.pc != 440) bad |= 32;
    if ((int)(unsigned long)&u->uc_mcontext.__reserved != 464) bad |= 64;
    return bad;
}
