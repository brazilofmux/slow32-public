#include <stdio.h>
#include <stdint.h>
#include "tube.h"

/*
 * fb prove-out: a 320x200 indexed pattern with a synthetic palette,
 * flipped once. The runner journals the RGBA hash against
 * expected.hash.
 */
#define W 320
#define H 200

static uint8_t pixels[W * H];
static uint32_t palette[256];

int main(void) {
    uint32_t info;
    int x, y, i;

    for (i = 0; i < 256; i++) {
        palette[i] = ((uint32_t)i << 16) |
                     ((uint32_t)(i ^ 0x55) << 8) |
                     (uint32_t)(255 - i);
    }
    for (y = 0; y < H; y++) {
        for (x = 0; x < W; x++) {
            pixels[y * W + x] = (uint8_t)((x * x + y * 3) & 0xFF);
        }
    }

    if (tube_init() != 0) {
        printf("no tube\n");
        return 1;
    }
    info = tube_info();
    if ((info & (1u << 1)) == 0) {
        printf("no fb\n");
        return 1;
    }
    if (tube_open_fb(W, H, pixels, palette) != 0) {
        printf("open-fail\n");
        return 1;
    }
    if (tube_flip(1u) != 0) {
        printf("flip-fail\n");
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
