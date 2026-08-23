/* sprites - the ppu demo: a scrolling starfield nametable and a
 * flock of bouncing balls, some of them half-alpha ghosts. q or ESC
 * on the glass quits. */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <tube.h>

#define NT_W 64
#define NT_H 32
#define NBALLS 24

static uint8_t pattern[1024 * 32];
static uint16_t nametable[NT_W * NT_H];
static uint32_t palettes[8][16];
static uint8_t oam[128][8];
static uint32_t regs[16];

static struct {
    int x, y, dx, dy;
} ball[NBALLS];

static uint32_t rng = 0x5EED5EEDu;
static uint32_t rnd(uint32_t n) {
    rng ^= rng << 13;
    rng ^= rng >> 17;
    rng ^= rng << 5;
    return (rng >> 8) % n;
}

static void set_pixel(int tile, int x, int y, int v) {
    uint8_t *b = &pattern[tile * 32 + y * 4 + x / 2];
    if (x & 1) {
        *b = (uint8_t)((*b & 0xF0) | (v & 0xF));
    } else {
        *b = (uint8_t)((*b & 0x0F) | ((v & 0xF) << 4));
    }
}

static void build_tiles(void) {
    int x, y;
    /* tile 1: a shaded ball (radius falloff) */
    for (y = 0; y < 8; y++) {
        for (x = 0; x < 8; x++) {
            int dx = x * 2 - 7, dy = y * 2 - 7;
            int d2 = dx * dx + dy * dy;
            int v = d2 >= 49 ? 0 : 15 - d2 * 12 / 49;
            set_pixel(1, x, y, v < 0 ? 1 : v);
        }
    }
    /* tile 2: a single star pixel; tile 3: a twinkle cross */
    set_pixel(2, 3, 3, 6);
    set_pixel(3, 3, 3, 15);
    set_pixel(3, 2, 3, 4);
    set_pixel(3, 4, 3, 4);
    set_pixel(3, 3, 2, 4);
    set_pixel(3, 3, 4, 4);
}

static void build_world(void) {
    int i, x, y;
    for (y = 0; y < NT_H; y++) {
        for (x = 0; x < NT_W; x++) {
            uint32_t r = rnd(100);
            uint16_t cell = 0;
            if (r < 8) {
                cell = 2;
            } else if (r < 10) {
                cell = 3;
            }
            nametable[y * NT_W + x] = cell;
        }
    }
    /* palette 0: starlight; 1..6: ball colors; 7: ghost */
    for (i = 0; i < 16; i++) {
        uint32_t v = (uint32_t)(i * 17);
        palettes[0][i] = 0xFF000000u | (v << 16) | (v << 8) | v;
        palettes[1][i] = 0xFF000000u | (v << 16) | ((v / 2) << 8);
        palettes[2][i] = 0xFF000000u | (v << 8) | (v / 3);
        palettes[3][i] = 0xFF000000u | (v << 16) | v;
        palettes[4][i] = 0xFF000000u | ((v / 2) << 16) | (v << 8) | v;
        palettes[5][i] = 0xFF000000u | (v << 16) | (v << 8) | (v / 4);
        palettes[6][i] = 0xFF000000u | ((v / 3) << 16) | ((v / 2) << 8) | v;
        palettes[7][i] = 0xFF000000u | (v << 16) | (v << 8) | v;
    }
    for (i = 0; i < NBALLS; i++) {
        ball[i].x = 20 + (int)rnd(280);
        ball[i].y = 20 + (int)rnd(160);
        ball[i].dx = ((int)rnd(2) ? 1 : -1) * (1 + (int)rnd(3));
        ball[i].dy = ((int)rnd(2) ? 1 : -1) * (1 + (int)rnd(3));
    }
}

static void step_balls(uint32_t frame) {
    int i;
    for (i = 0; i < NBALLS; i++) {
        int ghost = (i % 4) == 3;
        ball[i].x += ball[i].dx;
        ball[i].y += ball[i].dy;
        if (ball[i].x <= 0 || ball[i].x >= 312) {
            ball[i].dx = -ball[i].dx;
        }
        if (ball[i].y <= 0 || ball[i].y >= 192) {
            ball[i].dy = -ball[i].dy;
        }
        oam[i][0] = (uint8_t)(ball[i].x & 0xFF);
        oam[i][1] = (uint8_t)((ball[i].x >> 8) & 0xFF);
        oam[i][2] = (uint8_t)(ball[i].y & 0xFF);
        oam[i][3] = (uint8_t)((ball[i].y >> 8) & 0xFF);
        oam[i][4] = 1;  /* ball tile */
        oam[i][5] = (uint8_t)(((ghost ? 7 : 1 + (i % 6)) & 7) << 2);
        oam[i][6] = (uint8_t)(ghost ? 110 : 255);
        oam[i][7] = 1;
    }
    regs[6] = frame / 4;        /* drift the starfield */
    regs[7] = frame / 16;
}

int main(void) {
    struct {
        uint16_t code;
        uint8_t down, pad;
    } ev[8];
    uint32_t frame = 0;
    int done = 0, spins = 0;

    build_tiles();
    build_world();
    regs[0] = (uint32_t)(uintptr_t)pattern;
    regs[1] = (uint32_t)(uintptr_t)nametable;
    regs[2] = (uint32_t)(uintptr_t)oam;
    regs[3] = (uint32_t)(uintptr_t)palettes;
    regs[4] = NT_W;
    regs[5] = NT_H;
    regs[8] = 0xFF000418u;      /* deep space blue-black */

    if (tube_init() != 0) {
        printf("no tube: attach a screen\n");
        return 1;
    }
    if (tube_open_ppu(regs) != 0) {
        printf("open-fail\n");
        return 1;
    }
    printf("sprites: q or ESC on the glass quits\n");
    while ((tube_info() & (1u << 8)) == 0 && spins++ < 150) {
        usleep(20000);
    }

    while (!done) {
        int n, i;
        step_balls(frame);
        if (tube_flip(++frame) != 0) {
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
