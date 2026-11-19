/* The key queue overflows (docs/plans/input-queue.md, the third edge): it
 * holds 256 events and drops the oldest, and the oldest event kept then
 * says so -- TUBE_KEY_LOST in its flags byte -- so a guest that took a
 * key's down and lost its up can let go of what it thinks is held.
 *
 * inject.py sends 300 events, codes 0x200+i, in one write; event 100 has
 * 0xFE in its reserved byte.  The guest lets them all arrive, then reads:
 * 256 events, the first is 0x200+44 and flagged, none after it is, the
 * last is 0x200+299 -- and event 100's 0xFE was cleared, or it would
 * count as flagged: the byte is the host's. */
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include "tube.h"

#define VIEWER_BIT (1u << 8)

typedef struct { uint16_t code; uint8_t down; uint8_t flags; } keyev_t;

int main(void) {
    keyev_t ev[16];
    int total = 0, flagged = 0, spins = 0, n, i;
    unsigned first = 0, first_flags = 0, last = 0;

    if (tube_init() != 0 || tube_open(TUBE_MODE_VEC) != 0) {
        printf("no tube\n");
        return 1;
    }
    while ((tube_info() & VIEWER_BIT) == 0) {
        if (++spins > 100) { printf("no viewer\n"); return 1; }
        usleep(20000);
    }
    usleep(300000);                 /* the 300 events land before anything is read */

    while ((n = tube_keys(ev, (uint32_t)sizeof(ev))) > 0) {
        for (i = 0; i < n; i++, total++) {
            if (total == 0) { first = ev[i].code; first_flags = ev[i].flags; }
            else if (ev[i].flags) flagged++;
            last = ev[i].code;
        }
    }
    printf("read %d events (the queue holds 256)\n", total);
    printf("first 0x%03x, flags 0x%02x (0x%03x expected, TUBE_KEY_LOST)\n",
           first, first_flags, 0x200 + 44);
    printf("flagged after it: %d\n", flagged);
    printf("last 0x%03x\n", last);
    tube_cleanup();
    return 0;
}
