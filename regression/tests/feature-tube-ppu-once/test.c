#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "tube.h"

/*
 * ppu prove-out: two tiles, a flipped-cell nametable with scroll, and
 * three sprites -- one plain, one straddling the left edge (negative
 * x), one half-alpha overlapping the first. The runner journals the
 * composite hash against expected.hash.
 */
#define NT_W 64
#define NT_H 32

static uint8_t pattern[1024 * 32];
static uint16_t nametable[NT_W * NT_H];
static uint32_t palettes[8][16];
static uint8_t oam[128][8];
static uint32_t regs[16];

static void set_pixel(int tile, int x, int y, int v) {
    uint8_t *b = &pattern[tile * 32 + y * 4 + x / 2];
    if (x & 1) {
        *b = (uint8_t)((*b & 0xF0) | (v & 0xF));
    } else {
        *b = (uint8_t)((*b & 0x0F) | ((v & 0xF) << 4));
    }
}

static void put_sprite(int i, int x, int y, int tile, int pal,
                       int flips, int alpha) {
    oam[i][0] = (uint8_t)(x & 0xFF);
    oam[i][1] = (uint8_t)((x >> 8) & 0xFF);
    oam[i][2] = (uint8_t)(y & 0xFF);
    oam[i][3] = (uint8_t)((y >> 8) & 0xFF);
    oam[i][4] = (uint8_t)(tile & 0xFF);
    oam[i][5] = (uint8_t)(((tile >> 8) & 3) | ((pal & 7) << 2) | (flips << 5));
    oam[i][6] = (uint8_t)alpha;
    oam[i][7] = 1; /* enable */
}

int main(void) {
    int x, y, i;

    /* tile 1: diagonal ramp; tile 2: hollow box */
    for (y = 0; y < 8; y++) {
        for (x = 0; x < 8; x++) {
            set_pixel(1, x, y, ((x + y) % 3) + 1);
            set_pixel(2, x, y,
                      (x == 0 || y == 0 || x == 7 || y == 7) ? 5 : 0);
        }
    }
    for (y = 0; y < NT_H; y++) {
        for (x = 0; x < NT_W; x++) {
            uint16_t cell = (uint16_t)(((x + y) & 1) ? 1 : 2);
            cell |= (uint16_t)((x & 1) << 10);      /* palette 0/1 */
            if (y & 1) {
                cell |= 1u << 13;                    /* hflip */
            }
            if (x & 2) {
                cell |= 1u << 14;                    /* vflip */
            }
            nametable[y * NT_W + x] = cell;
        }
    }
    for (i = 0; i < 16; i++) {
        palettes[0][i] = 0xFF000000u | ((uint32_t)(i * 16) << 16) | 0x2040u;
        palettes[1][i] = 0xFF000000u | 0x400000u | ((uint32_t)(i * 16) << 8);
        palettes[2][i] = 0x80FF8000u | (uint32_t)(i * 15);  /* half-alpha pal */
    }

    put_sprite(0, 60, 40, 1, 2, 0, 255);
    put_sprite(1, -4, 100, 2, 1, 0, 255);   /* straddles the left edge */
    put_sprite(2, 63, 43, 2, 0, 3, 128);    /* overlaps sprite 0, both flips */

    regs[0] = (uint32_t)(uintptr_t)pattern;
    regs[1] = (uint32_t)(uintptr_t)nametable;
    regs[2] = (uint32_t)(uintptr_t)oam;
    regs[3] = (uint32_t)(uintptr_t)palettes;
    regs[4] = NT_W;
    regs[5] = NT_H;
    regs[6] = 5;    /* scroll_x */
    regs[7] = 9;    /* scroll_y */
    regs[8] = 0xFF101020u;
    regs[9] = 0;

    if (tube_init() != 0) {
        printf("no tube\n");
        return 1;
    }
    if ((tube_info() & (1u << 2)) == 0) {
        printf("no ppu\n");
        return 1;
    }
    if (tube_open_ppu(regs) != 0) {
        printf("open-fail\n");
        return 1;
    }
    if (tube_flip(1u) != 0) {
        printf("flip-fail\n");
        return 1;
    }
    printf("present-ok\n");
    tube_cleanup();
    return 0;
}
