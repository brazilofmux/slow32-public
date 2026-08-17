#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include "tube.h"

#define VIEWER_BIT (1u << 8)

static uint32_t list[] = {
    TUBE_WORD(TUBE_OP_MOVE, 0, 0),
    TUBE_WORD(TUBE_OP_DRAW, 8, 8),
    TUBE_WORD(TUBE_OP_END, 0, 0),
};

/* A viewer that dumps several KEYE frames in one send must deliver
   every event, not just the first. */
int main(void) {
    typedef struct {
        uint16_t code;
        uint8_t down;
        uint8_t pad;
    } keyev_t;
    keyev_t ev[16];
    keyev_t got[16];
    int ngot = 0;
    int spins = 0;
    int frames;
    int i;

    if (tube_init() != 0) {
        printf("no tube\n");
        return 1;
    }
    if (tube_open(TUBE_MODE_VEC) != 0) {
        printf("open-fail\n");
        return 1;
    }
    while ((tube_info() & VIEWER_BIT) == 0) {
        if (++spins > 100) {
            printf("no viewer\n");
            return 1;
        }
        usleep(20000);
    }

    for (frames = 0; frames < 16 && ngot < 16; frames++) {
        int n = tube_keys(ev, (uint32_t)sizeof(ev));
        int k;
        if (n < 0) {
            printf("keys-fail\n");
            return 1;
        }
        for (k = 0; k < n && ngot < 16; k++) {
            got[ngot++] = ev[k];
        }
        if (tube_present(list, 3, (uint32_t)frames) != 0) {
            printf("present-fail\n");
            return 1;
        }
        usleep(10000);
    }

    printf("keys");
    for (i = 0; i < ngot; i++) {
        printf(" %04x:%u", got[i].code, (unsigned)got[i].down);
    }
    printf("\n");
    tube_cleanup();
    return 0;
}
