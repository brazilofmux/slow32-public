/* The tube's key queue in a wait-for-any (docs/plans/input-queue.md, the
 * second edge): S32_FD_TUBE_KEYS beside fds in s32_dpc_wait_on.  A tube
 * program waits for "a key or the next tick" in one call, as a terminal
 * program does with fd 0.
 *
 *  1. No tube session yet: the name is NVAL, at once.
 *  2. Opened, nothing queued, a 30 s timer as the backstop: inject.py
 *     connects as a viewer and sends three KEYE events after HELO, and the
 *     wait wakes for them (the viewer's socket is in the wait), not for
 *     the timer.  The keys are still read with tube_keys().
 *  3. The queue drained, a 50 ms timer wins: readiness is the queue's
 *     state now, not a latch. */
#include <stdio.h>
#include <stdint.h>
#include "tube.h"
#include "s32dpc.h"

typedef struct { uint16_t code; uint8_t down; uint8_t pad; } keyev_t;

int main(void) {
    int fd = S32_FD_TUBE_KEYS;
    s32_dpc_t d;
    keyev_t ev[16];
    int n, i;

    n = s32_dpc_wait_on(&fd, 1, &d);
    printf("no tube: %s\n", n == 1 && d.kind == S32_DPC_READY && (int)d.id == fd &&
           (d.cookie & S32_DPC_NVAL) ? "NVAL" : "wrong");

    if (tube_init() != 0 || tube_open(TUBE_MODE_VEC) != 0) {
        printf("no tube\n");
        return 1;
    }

    int backstop = s32_timer_start(30u, 0u, 1u);
    n = s32_dpc_wait_on(&fd, 1, &d);
    if (n == 1 && d.kind == S32_DPC_READY && (int)d.id == fd)
        printf("woke for keys: %s\n", (d.cookie & S32_DPC_IN) ? "IN" : "?");
    else
        printf("woke for something else: kind %u id %u\n", d.kind, d.id);
    s32_timer_cancel(backstop);

    n = tube_keys(ev, (uint32_t)sizeof(ev));
    printf("keys");
    for (i = 0; i < n; i++) printf(" %04x:%u", ev[i].code, (unsigned)ev[i].down);
    printf("\n");

    s32_timer_start(0u, 50000000u, 2u);
    n = s32_dpc_wait_on(&fd, 1, &d);
    printf("drained, then: %s\n", n == 1 && d.kind == S32_DPC_TIMER && d.cookie == 2u
           ? "the timer" : "keys again (wrong)");

    tube_cleanup();
    return 0;
}
