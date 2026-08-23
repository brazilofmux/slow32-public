#include "graphics.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <tube.h>

static int tube_state = 0;      /* 0=untried, 1=granted, -1=denied */
static int warned = 0;

static int scr_mode = 0;        /* 0 = no graphics screen */
static int scr_w, scr_h, scr_colors;
static uint8_t *pixels = NULL;
static uint32_t palette[256];
static uint32_t generation = 1;
static int fg = 15;             /* default draw color */
static int last_x, last_y;

/* The 16 RGBI colors every 80s screen agreed on. */
static const uint32_t rgbi16[16] = {
    0x000000, 0x0000AA, 0x00AA00, 0x00AAAA,
    0xAA0000, 0xAA00AA, 0xAA5500, 0xAAAAAA,
    0x555555, 0x5555FF, 0x55FF55, 0x55FFFF,
    0xFF5555, 0xFF55FF, 0xFFFF55, 0xFFFFFF,
};

static void build_default_palette(void) {
    int i;
    memset(palette, 0, sizeof(palette));
    for (i = 0; i < 16; i++)
        palette[i] = rgbi16[i];
    if (scr_colors <= 16)
        return;
    /* 256-color mode: 16 RGBI, a 16-step gray ramp, a 6x6x6 cube,
     * and black in the tail. Not the VGA ROM palette; PALETTE can
     * make it anything. */
    for (i = 16; i < 32; i++) {
        uint32_t v = (uint32_t)(i - 16) * 17u;
        palette[i] = (v << 16) | (v << 8) | v;
    }
    for (i = 32; i < 248; i++) {
        int idx = i - 32;
        uint32_t r = (uint32_t)(idx / 36) * 51u;
        uint32_t g = (uint32_t)((idx / 6) % 6) * 51u;
        uint32_t b = (uint32_t)(idx % 6) * 51u;
        palette[i] = (r << 16) | (g << 8) | b;
    }
}

static void flip(void) {
    if (scr_mode)
        tube_flip(generation++);
}

static void degrade(void) {
    if (!warned) {
        printf("No tube: graphics unavailable\n");
        warned = 1;
    }
    tube_state = -1;
}

int sb_gfx_screen(int mode) {
    int w, h, colors;

    if (mode == 0) {
        if (scr_mode) {
            tube_close();
            free(pixels);
            pixels = NULL;
            scr_mode = 0;
        }
        return 0;
    }

    switch (mode) {
        case 1: case 7:  w = 320; h = 200; colors = 16;  break;
        case 2: case 8:  w = 640; h = 200; colors = 16;  break;
        case 9:          w = 640; h = 350; colors = 16;  break;
        case 12:         w = 640; h = 480; colors = 16;  break;
        case 13:         w = 320; h = 200; colors = 256; break;
        default:         return -2;
    }

    if (tube_state == 0)
        tube_state = (tube_init() == 0) ? 1 : -1;
    if (tube_state != 1) {
        degrade();
        return -1;
    }

    if (scr_mode) {
        tube_close();
        free(pixels);
        pixels = NULL;
        scr_mode = 0;
    }

    pixels = calloc(1, (size_t)w * (size_t)h);
    if (!pixels) {
        degrade();
        return -1;
    }
    scr_w = w;
    scr_h = h;
    scr_colors = colors;
    build_default_palette();
    if (tube_open_fb((uint32_t)w, (uint32_t)h, pixels, palette) != 0) {
        free(pixels);
        pixels = NULL;
        degrade();
        return -1;
    }
    scr_mode = mode;
    fg = 15;
    last_x = w / 2;
    last_y = h / 2;
    flip();
    return 0;
}

int sb_gfx_active(void)   { return scr_mode != 0; }
int sb_gfx_degraded(void) { return tube_state == -1; }
int sb_gfx_width(void)    { return scr_w; }
int sb_gfx_height(void)   { return scr_h; }

void sb_gfx_last(int *x, int *y) {
    *x = last_x;
    *y = last_y;
}

static int clamp_color(int color) {
    if (color < 0)
        return fg;
    return color & (scr_colors - 1);
}

static void plot(int x, int y, int c) {
    if (x < 0 || x >= scr_w || y < 0 || y >= scr_h)
        return;
    pixels[y * scr_w + x] = (uint8_t)c;
}

void sb_gfx_pset(int x, int y, int color) {
    if (!scr_mode)
        return;
    plot(x, y, clamp_color(color));
    last_x = x;
    last_y = y;
    flip();
}

static void hspan(int x0, int x1, int y, int c) {
    int x;
    if (x0 > x1) { x = x0; x0 = x1; x1 = x; }
    for (x = x0; x <= x1; x++)
        plot(x, y, c);
}

static void bresenham(int x0, int y0, int x1, int y1, int c) {
    int dx = x1 > x0 ? x1 - x0 : x0 - x1;
    int dy = y1 > y0 ? y1 - y0 : y0 - y1;
    int sx = x0 < x1 ? 1 : -1;
    int sy = y0 < y1 ? 1 : -1;
    int err = dx - dy;
    for (;;) {
        plot(x0, y0, c);
        if (x0 == x1 && y0 == y1)
            break;
        int e2 = 2 * err;
        if (e2 > -dy) { err -= dy; x0 += sx; }
        if (e2 <  dx) { err += dx; y0 += sy; }
    }
}

void sb_gfx_line(int x0, int y0, int x1, int y1, int color, int box) {
    int c, y;
    if (!scr_mode)
        return;
    c = clamp_color(color);
    if (box == 0) {
        bresenham(x0, y0, x1, y1, c);
    } else if (box == 1) {
        hspan(x0, x1, y0, c);
        hspan(x0, x1, y1, c);
        bresenham(x0, y0, x0, y1, c);
        bresenham(x1, y0, x1, y1, c);
    } else {
        int ylo = y0 < y1 ? y0 : y1;
        int yhi = y0 < y1 ? y1 : y0;
        for (y = ylo; y <= yhi; y++)
            hspan(x0, x1, y, c);
    }
    last_x = x1;
    last_y = y1;
    flip();
}

void sb_gfx_circle(int cx, int cy, int r, int color) {
    int c, x, y, err;
    if (!scr_mode)
        return;
    c = clamp_color(color);
    if (r < 0)
        r = -r;
    x = r;
    y = 0;
    err = 1 - r;
    while (x >= y) {
        plot(cx + x, cy + y, c);
        plot(cx - x, cy + y, c);
        plot(cx + x, cy - y, c);
        plot(cx - x, cy - y, c);
        plot(cx + y, cy + x, c);
        plot(cx - y, cy + x, c);
        plot(cx + y, cy - x, c);
        plot(cx - y, cy - x, c);
        y++;
        if (err < 0) {
            err += 2 * y + 1;
        } else {
            x--;
            err += 2 * (y - x) + 1;
        }
    }
    last_x = cx;
    last_y = cy;
    flip();
}

/* PAINT: seed fill bounded by the border color. Painted pixels stop
 * the walk too (except when paint == border, where the paint itself
 * is the boundary), so the fill terminates. Span-based seed stack. */
typedef struct { int x, y; } seed_t;

static int fillable(int x, int y, int paint, int border) {
    uint8_t p;
    if (x < 0 || x >= scr_w || y < 0 || y >= scr_h)
        return 0;
    p = pixels[y * scr_w + x];
    if (p == (uint8_t)border)
        return 0;
    if (paint != border && p == (uint8_t)paint)
        return 0;
    return 1;
}

void sb_gfx_paint(int x, int y, int paint, int border) {
    seed_t *stack;
    int top = 0, cap = 1024;

    if (!scr_mode)
        return;
    paint = clamp_color(paint);
    border = (border < 0) ? paint : (border & (scr_colors - 1));
    last_x = x;
    last_y = y;
    if (!fillable(x, y, paint, border)) {
        flip();
        return;
    }
    stack = malloc(cap * sizeof(seed_t));
    if (!stack)
        return;
    stack[top].x = x;
    stack[top].y = y;
    top = 1;
    while (top > 0) {
        int lx, rx, cy, row, sx;
        top--;
        x = stack[top].x;
        cy = stack[top].y;
        if (!fillable(x, cy, paint, border))
            continue;
        lx = x;
        while (fillable(lx - 1, cy, paint, border))
            lx--;
        rx = x;
        while (fillable(rx + 1, cy, paint, border))
            rx++;
        for (sx = lx; sx <= rx; sx++)
            pixels[cy * scr_w + sx] = (uint8_t)paint;
        for (row = cy - 1; row <= cy + 1; row += 2) {
            for (sx = lx; sx <= rx; sx++) {
                if (fillable(sx, row, paint, border)) {
                    /* push the start of each run once */
                    if (sx == lx || !fillable(sx - 1, row, paint, border)) {
                        if (top == cap) {
                            seed_t *ns;
                            cap *= 2;
                            ns = realloc(stack, cap * sizeof(seed_t));
                            if (!ns) {
                                free(stack);
                                flip();
                                return;
                            }
                            stack = ns;
                        }
                        stack[top].x = sx;
                        stack[top].y = row;
                        top++;
                    }
                }
            }
        }
    }
    free(stack);
    flip();
}

void sb_gfx_cls(void) {
    if (!scr_mode)
        return;
    memset(pixels, 0, (size_t)scr_w * (size_t)scr_h);
    flip();
}

void sb_gfx_palette(int attr, int rgb) {
    if (!scr_mode)
        return;
    if (attr < 0) {
        build_default_palette();
    } else {
        palette[attr & (scr_colors - 1)] = (uint32_t)rgb & 0xFFFFFFu;
    }
    flip();
}

int sb_gfx_point(int x, int y) {
    if (!scr_mode || x < 0 || x >= scr_w || y < 0 || y >= scr_h)
        return -1;
    return pixels[y * scr_w + x];
}

/* --- viewer keys -> INKEY$ --- */

static uint8_t evq[64][4];
static int evq_n = 0, evq_i = 0;

int sb_gfx_inkey(char out[3]) {
    if (!scr_mode || tube_state != 1)
        return 0;
    for (;;) {
        if (evq_i >= evq_n) {
            int n;
            evq_i = evq_n = 0;
            n = tube_keys(evq, (uint32_t)sizeof(evq));
            if (n <= 0)
                return 0;
            evq_n = n;
        }
        while (evq_i < evq_n) {
            uint8_t *e = evq[evq_i++];
            unsigned code = (unsigned)e[0] | ((unsigned)e[1] << 8);
            int sc = 0;
            if (!e[2])
                continue;       /* key-up */
            if (code >= 1 && code < 256) {
                out[0] = (char)code;
                out[1] = '\0';
                return 1;
            }
            switch (code) {     /* GW extended keys: CHR$(0) + scancode */
                case 0x100: sc = 72; break;             /* up */
                case 0x101: sc = 80; break;             /* down */
                case 0x102: sc = 75; break;             /* left */
                case 0x103: sc = 77; break;             /* right */
                default:
                    if (code >= 0x108 && code <= 0x111) /* F1..F10 */
                        sc = 59 + (int)(code - 0x108);
                    break;
            }
            if (sc) {
                out[0] = '\0';
                out[1] = (char)sc;
                out[2] = '\0';
                return 2;
            }
        }
    }
}
