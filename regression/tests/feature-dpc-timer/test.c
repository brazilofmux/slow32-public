/* The interrupt-to-DPC path, end to end: one timer source, one DPC, one
 * instance observing it (docs/plans/dpc.md).  The output is deterministic
 * although the clock is not: the guest observes a timer only by waiting
 * for it or by looking after a sleep longer than its interval, never by
 * counting. */
#include <stdio.h>
#include <unistd.h>
#include "s32dpc.h"

int main(void) {
    s32_dpc_t d;

    if (s32_dpc_poll(&d)) printf("BAD: an entry before anything was armed\n");
    if (s32_dpc_wait(&d) < 0) printf("wait with nothing armed: nothing can arrive\n");

    /* two timers, armed long first, fire short first */
    int a = s32_timer_start(0, 20000000u, 0xA1);
    int b = s32_timer_start(0, 5000000u, 0xB2);
    printf("armed ids %d %d\n", a, b);

    /* work without yielding, then look */
    volatile unsigned x = 0;
    for (unsigned i = 0; i < 100000u; i++) x += i;

    for (int n = 0; n < 2; n++) {
        if (s32_dpc_wait(&d) == 1) printf("dpc kind=%x id=%u cookie=%x\n", d.kind, d.id, d.cookie);
        else printf("BAD: wait failed\n");
    }
    if (s32_dpc_poll(&d)) printf("BAD: a third entry\n"); else printf("queue empty\n");

    /* a cancelled timer never arrives */
    int c = s32_timer_start(0, 5000000u, 0xC3);
    if (s32_timer_cancel(c) == 0 && s32_dpc_wait(&d) < 0) printf("cancelled: nothing can arrive\n");

    /* a timer that expires while the instance is asleep in the host for
     * another reason is queued before the instance resumes */
    s32_timer_start(0, 1000000u, 0xE5);
    usleep(5000);
    if (s32_dpc_poll(&d)) printf("seen without waiting: cookie=%x\n", d.cookie);
    else printf("BAD: not queued after a longer sleep\n");

    /* the ids are a fixed partition: once fired, a slot is free again */
    int e = s32_timer_start(0, 1000000u, 0xF6);
    printf("reused id %d\n", e);
    s32_dpc_wait(&d);
    printf("done cookie=%x\n", d.cookie);
    return 0;
}
