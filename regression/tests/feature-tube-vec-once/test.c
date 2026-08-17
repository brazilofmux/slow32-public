#include <stdio.h>
#include <stdint.h>
#include "tube.h"

/*
 * Present-once prove-out: a box, then a dimmer point. No sleep, no keys,
 * no viewer. The runner journals the dump hash against expected.hash.
 */
static const uint32_t list[] = {
    TUBE_WORD(TUBE_OP_MOVE, 100, 100),
    TUBE_WORD(TUBE_OP_DRAW, 300, 100),
    TUBE_WORD(TUBE_OP_DRAW, 300, 300),
    TUBE_WORD(TUBE_OP_DRAW, 100, 300),
    TUBE_WORD(TUBE_OP_DRAW, 100, 100),
    TUBE_WORD_INTEN(128),
    TUBE_WORD(TUBE_OP_POINT, 200, 200),
    TUBE_WORD(TUBE_OP_END, 0, 0),
};

int main(void) {
    uint32_t info;

    if (tube_init() != 0) {
        printf("no tube\n");
        return 1;
    }
    info = tube_info();
    if ((info & 1u) == 0) {
        printf("no vec\n");
        return 1;
    }
    if (tube_open(TUBE_MODE_VEC) != 0) {
        printf("open-fail\n");
        return 1;
    }
    if (tube_present(list, (uint32_t)(sizeof(list) / sizeof(list[0])), 1u) != 0) {
        printf("present-fail\n");
        return 1;
    }
    if ((tube_status() & 0xFFFFFFu) != 1u) {
        printf("status-fail\n");
        return 1;
    }
    printf("present-ok\n");
    tube_cleanup();
    return 0;
}
