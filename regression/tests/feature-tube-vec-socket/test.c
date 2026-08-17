#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include "tube.h"

#define VIEWER_BIT (1u << 8)

/* Not const: a following .rodata.str can leave a const array unaligned. */
static uint32_t list[] = {
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
    int spins = 0;

    if (tube_init() != 0) {
        printf("no tube\n");
        return 1;
    }
    if (tube_open(TUBE_MODE_VEC) != 0) {
        printf("open-fail\n");
        return 1;
    }
    while ((tube_info() & VIEWER_BIT) == 0) {
        if (++spins > 50) {
            printf("no viewer\n");
            return 1;
        }
        usleep(20000);
    }
    if (tube_present(list, (uint32_t)(sizeof(list) / sizeof(list[0])), 1u) != 0) {
        printf("present-fail\n");
        return 1;
    }
    printf("present-ok\n");
    tube_cleanup();
    return 0;
}
