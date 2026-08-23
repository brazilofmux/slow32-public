/* fire - the classic demoscene fire, on the tube's framebuffer.
 * 320x200 P8 with the traditional black->red->yellow->white palette;
 * the palette is re-read every flip, but this one earns its keep the
 * honest way: by moving pixels. q or ESC on the glass quits. */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <tube.h>

#define W 320
#define H 200

static uint8_t heat[(H + 2) * W];   /* two hidden seed rows below */
static uint8_t pixels[W * H];
static uint32_t palette[256];

static uint32_t rng = 0xF12EBA11u;
static uint32_t rnd(void) {
    rng ^= rng << 13;
    rng ^= rng >> 17;
    rng ^= rng << 5;
    return rng;
}

static void build_palette(void) {
    int i;
    for (i = 0; i < 256; i++) {
        int r, g, b;
        if (i < 96) {           /* black -> red */
            r = i * 255 / 95;
            g = 0;
            b = 0;
        } else if (i < 192) {   /* red -> yellow */
            r = 255;
            g = (i - 96) * 255 / 95;
            b = 0;
        } else {                /* yellow -> white */
            r = 255;
            g = 255;
            b = (i - 192) * 255 / 63;
        }
        palette[i] = ((uint32_t)r << 16) | ((uint32_t)g << 8) | (uint32_t)b;
    }
}

static void step_fire(void) {
    int x, y;
    /* Seed the hidden rows: mostly embers, some white-hot. */
    for (y = H; y < H + 2; y++) {
        for (x = 0; x < W; x++) {
            heat[y * W + x] = (uint8_t)((rnd() & 1u) ? 255 : 32);
        }
    }
    for (y = H - 1; y >= 0; y--) {
        for (x = 0; x < W; x++) {
            int xl = x > 0 ? x - 1 : W - 1;
            int xr = x < W - 1 ? x + 1 : 0;
            int v = heat[(y + 1) * W + xl] + heat[(y + 1) * W + x] +
                    heat[(y + 1) * W + xr] + heat[(y + 2) * W + x];
            v = v * 63 / 258;   /* average with a little cooling */
            heat[y * W + x] = (uint8_t)v;
        }
    }
    memcpy(pixels, heat, sizeof(pixels));
}

int main(void) {
    struct {
        uint16_t code;
        uint8_t down, pad;
    } ev[8];
    uint32_t gen = 1;
    int done = 0, spins = 0;

    build_palette();
    if (tube_init() != 0) {
        printf("no tube: attach a screen\n");
        return 1;
    }
    if (tube_open_fb(W, H, pixels, palette) != 0) {
        printf("open-fail\n");
        return 1;
    }
    printf("fire: q or ESC on the glass quits\n");
    while ((tube_info() & (1u << 8)) == 0 && spins++ < 150) {
        usleep(20000);
    }

    while (!done) {
        int n, i;
        step_fire();
        if (tube_flip(gen++) != 0) {
            printf("flip-fail\n");
            break;
        }
        n = tube_keys(ev, (uint32_t)sizeof(ev));
        for (i = 0; i < n; i++) {
            if (ev[i].down &&
                (ev[i].code == 'q' || ev[i].code == 'Q' || ev[i].code == 27)) {
                done = 1;
            }
        }
        usleep(33000);
    }
    tube_cleanup();
    return 0;
}
