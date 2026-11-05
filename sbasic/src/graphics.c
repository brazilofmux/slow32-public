#include "graphics.h"
#include "machine.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#include <tube.h>

static int tube_state = 0;      /* 0=untried, 1=granted, -1=denied */
static int warned = 0;

static int scr_mode = 0;        /* 0 = no graphics screen */
static int scr_w, scr_h, scr_colors;
static uint8_t *pixels = NULL;
static uint32_t palette[256];
static uint32_t generation = 1;
static int fg = 15;             /* default draw color */
static void draw_reset(void);
static void flip(void);

/* VIEW: the viewport, absolute and inclusive (the whole screen when
 * none); view_scr, VIEW SCREEN's absolute coordinates (and none's), else
 * VIEW's, relative to the viewport's corner.  WINDOW: the world mapped
 * onto it, y up unless WINDOW SCREEN.  The last point is the world's
 * (QBasic keeps it so: STEP(.5, .5) from 1, 1 is 1.5, 1.5 exactly). */
static int vx1, vy1, vx2, vy2, view_scr = 1;
static int win_on, win_scr;
static double wx1, wy1, wx2, wy2;
static double last_lx, last_ly;

static void view_reset(void) {
    vx1 = 0; vy1 = 0; vx2 = scr_w - 1; vy2 = scr_h - 1; view_scr = 1;
}

/* The 16 RGBI colors every 80s screen agreed on. */
static const uint32_t rgbi16[16] = {
    0x000000, 0x0000AA, 0x00AA00, 0x00AAAA,
    0xAA0000, 0xAA00AA, 0xAA5500, 0xAAAAAA,
    0x555555, 0x5555FF, 0x55FF55, 0x55FFFF,
    0xFF5555, 0xFF55FF, 0xFFFF55, 0xFFFFFF,
};

/* The VGA's colors: a DAC of 256 (here 8 bits a component; the ports
 * read and write 6), and in the 16-color modes the attribute controller's
 * map of each attribute to a DAC entry -- loaded, as the BIOS loads them,
 * with EGA's 64 colors and 0-5, 20, 7, 56-63 (attribute 6 is brown,
 * DAC 20).  What the screen shows (palette[]) is made from them. */
static uint32_t dac[256];
static uint8_t attr_map[16];

static void palette_sync(void) {
    for (int i = 0; i < 256; i++)
        palette[i] = scr_colors <= 16 ? (i < 16 ? dac[attr_map[i]] : 0) : dac[i];
}

/* EGA's 64 colors: bits 0-2 B G R at 2/3, bits 3-5 b g r at 1/3 */
static uint32_t ega64(int k) {
    uint32_t r = (k & 4 ? 0xAA : 0) + (k & 32 ? 0x55 : 0);
    uint32_t g = (k & 2 ? 0xAA : 0) + (k & 16 ? 0x55 : 0);
    uint32_t b = (k & 1 ? 0xAA : 0) + (k & 8 ? 0x55 : 0);
    return r << 16 | g << 8 | b;
}

/* the 16 colors' EGA registers by default (6 brown, 8 to 15 bright) */
static const uint8_t map16[16] = { 0, 1, 2, 3, 4, 5, 20, 7, 56, 57, 58, 59, 60, 61, 62, 63 };

static void build_default_palette(void) {
    int i;
    memset(dac, 0, sizeof(dac));
    if (scr_colors <= 16) {
        for (i = 0; i < 64; i++) dac[i] = ega64(i);
        memcpy(attr_map, map16, sizeof attr_map);
        if (scr_colors == 4) {          /* (CGA's: bright cyan, magenta, white) */
            attr_map[1] = 59; attr_map[2] = 61; attr_map[3] = 63;
        } else if (scr_colors == 2) {
            attr_map[1] = 63;
        }
        palette_sync();
        return;
    }
    for (i = 0; i < 16; i++)
        dac[i] = rgbi16[i];
    /* 256-color mode: 16 RGBI, a 16-step gray ramp, a 6x6x6 cube,
     * and black in the tail. Not the VGA ROM palette; PALETTE can
     * make it anything. */
    for (i = 16; i < 32; i++) {
        uint32_t v = (uint32_t)(i - 16) * 17u;
        dac[i] = (v << 16) | (v << 8) | v;
    }
    for (i = 32; i < 248; i++) {
        int idx = i - 32;
        uint32_t r = (uint32_t)(idx / 36) * 51u;
        uint32_t g = (uint32_t)((idx / 6) % 6) * 51u;
        uint32_t b = (uint32_t)(idx % 6) * 51u;
        dac[i] = (r << 16) | (g << 8) | b;
    }
    palette_sync();
}

/* the DAC's entry k, component c (0 red, 1 green, 2 blue), 6 bits, as
 * port 3C9 reads and writes it */
int sb_gfx_dac_get(int k, int c) {
    return (int)(dac[k & 255] >> (16 - 8 * c) & 0xFF) >> 2;
}

void sb_gfx_dac_set(int k, int r6, int g6, int b6) {
    uint32_t r = (uint32_t)(r6 & 63), g = (uint32_t)(g6 & 63), b = (uint32_t)(b6 & 63);
    dac[k & 255] = (r << 2 | r >> 4) << 16 | (g << 2 | g >> 4) << 8 | (b << 2 | b >> 4);
    if (!scr_mode) return;
    palette_sync();
    flip();
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
        /* (the colors each has: 4 in 1, 2 in 2 and 11 -- PSET 15 there
         * reads back 3 and 1, QBasic) */
        case 1:          w = 320; h = 200; colors = 4;   break;
        case 7:          w = 320; h = 200; colors = 16;  break;
        case 2:          w = 640; h = 200; colors = 2;   break;
        case 8:          w = 640; h = 200; colors = 16;  break;
        case 9:          w = 640; h = 350; colors = 16;  break;
        case 11:         w = 640; h = 480; colors = 2;   break;
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
    fg = colors > 16 ? 15 : colors - 1;  /* (the foreground: the last attribute) */
    view_reset();
    win_on = 0;
    draw_reset();
    last_lx = w / 2;
    last_ly = h / 2;
    flip();
    return 0;
}

int sb_gfx_active(void)   { return scr_mode != 0; }
int sb_gfx_degraded(void) { return tube_state == -1; }
int sb_gfx_width(void)    { return scr_w; }
int sb_gfx_height(void)   { return scr_h; }

void sb_gfx_set_last(double x, double y) {
    last_lx = x;
    last_ly = y;
}

void sb_gfx_last(double *x, double *y) {
    *x = last_lx;
    *y = last_ly;
}

static int clamp_color(int color);

/* --- VIEW and WINDOW: the world's coordinates and the screen's ------- */

/* the viewport's corner the coordinates count from */
static int org_x(void) { return view_scr ? 0 : vx1; }
static int org_y(void) { return view_scr ? 0 : vy1; }

/* CINT's rounding, half to even; 0 when it does not fit an int */
static int cint(double v, int *out) {
    if (!(v > -2147483648.5 && v < 2147483647.5)) return 0;
    *out = (int)rint(v);
    return 1;
}

/* a world coordinate as the view's (PMAP 0 and 1), unrounded */
static double to_view(int axis, double v) {
    if (!win_on) return v;
    if (axis == 0) return (v - wx1) * (vx2 - vx1) / (wx2 - wx1) + (vx1 - org_x());
    return (win_scr ? v - wy1 : wy2 - v) * (vy2 - vy1) / (wy2 - wy1) + (vy1 - org_y());
}

/* a view coordinate as the world's (PMAP 2 and 3) */
static double to_world(int axis, double p) {
    if (!win_on) return p;
    if (axis == 0) return wx1 + (p - (vx1 - org_x())) * (wx2 - wx1) / (vx2 - vx1);
    double q = (p - (vy1 - org_y())) * (wy2 - wy1) / (vy2 - vy1);
    return win_scr ? wy1 + q : wy2 - q;
}

int sb_gfx_map(double x, double y, int *ax, int *ay) {
    int px, py;
    if (!cint(to_view(0, x), &px) || !cint(to_view(1, y), &py)) return -1;
    *ax = px + org_x();
    *ay = py + org_y();
    return 0;
}

int sb_gfx_windowed(void) { return win_on; }

double sb_gfx_xscale(void) { return win_on ? (vx2 - vx1) / (wx2 - wx1) : 1.0; }

int sb_gfx_pmap(double v, int n, double *out) {
    int p;
    if (n >= 2) {                       /* (a pixel: whole, first -- QBasic) */
        if (!cint(v, &p)) return -1;
        *out = to_world(n - 2, p);
        return 0;
    }
    if (!cint(to_view(n, v), &p)) return -1;
    *out = p;
    return 0;
}

int sb_gfx_point_n(int n, double *out) {
    if (n >= 2) { *out = n == 2 ? last_lx : last_ly; return 0; }
    return sb_gfx_pmap(n == 0 ? last_lx : last_ly, n, out);
}

/* the screen's pixel x, y as the world's last point */
static void last_at(int ax, int ay) {
    last_lx = to_world(0, ax - org_x());
    last_ly = to_world(1, ay - org_y());
}

int sb_gfx_view(int on, int scr, int x1, int y1, int x2, int y2, int fill, int border) {
    int t;
    if (!scr_mode) return 0;
    if (!on) {
        view_reset();
    } else {
        if (x1 > x2) { t = x1; x1 = x2; x2 = t; }
        if (y1 > y2) { t = y1; y1 = y2; y2 = t; }
        if (x1 < 0 || y1 < 0 || x2 >= scr_w || y2 >= scr_h || x1 == x2 || y1 == y2) return -1;
        vx1 = x1; vy1 = y1; vx2 = x2; vy2 = y2; view_scr = scr;
        if (fill >= 0)
            for (int y = y1; y <= y2; y++)
                memset(pixels + y * scr_w + x1, clamp_color(fill), (size_t)(x2 - x1 + 1));
        if (border >= 0) {              /* (a pixel outside the viewport, on the screen) */
            int c = clamp_color(border);
            for (int x = x1 - 1; x <= x2 + 1; x++)
                for (int y = y1 - 1; y <= y2 + 1; y += y2 - y1 + 2)
                    if (x >= 0 && x < scr_w && y >= 0 && y < scr_h) pixels[y * scr_w + x] = (uint8_t)c;
            for (int y = y1; y <= y2; y++)
                for (int x = x1 - 1; x <= x2 + 1; x += x2 - x1 + 2)
                    if (x >= 0 && x < scr_w) pixels[y * scr_w + x] = (uint8_t)c;
        }
    }
    {                                   /* the last point the viewport's middle */
        int cx, cy;
        cint((vx2 - vx1) / 2.0, &cx);
        cint((vy2 - vy1) / 2.0, &cy);
        last_at(vx1 + cx, vy1 + cy);
    }
    flip();
    return 0;
}

int sb_gfx_window(int on, int scr, double x1, double y1, double x2, double y2) {
    int ax, ay;
    double t;
    if (!scr_mode) return 0;
    if (on && (x1 == x2 || y1 == y2)) return -1;
    if (sb_gfx_map(last_lx, last_ly, &ax, &ay) < 0) { ax = vx1; ay = vy1; }
    win_on = on;
    if (on) {
        if (x1 > x2) { t = x1; x1 = x2; x2 = t; }
        if (y1 > y2) { t = y1; y1 = y2; y2 = t; }
        wx1 = x1; wy1 = y1; wx2 = x2; wy2 = y2; win_scr = scr;
    }
    last_at(ax, ay);                    /* (the same pixel, in the new world) */
    return 0;
}

/* a color as QBasic draws it: its low byte, the mode's last color if
 * past them (SCREEN 1's 4 to 255 all 3; -1 given is the foreground) */
static int mode_color(int color) {
    color &= 255;
    return color >= scr_colors ? scr_colors - 1 : color;
}

static int clamp_color(int color) {
    if (color < 0)
        return fg;
    return mode_color(color);
}

static void plot(int x, int y, int c) {
    if (x < vx1 || x > vx2 || y < vy1 || y > vy2)
        return;
    pixels[y * scr_w + x] = (uint8_t)c;
}

void sb_gfx_pset(int x, int y, int color) {
    if (!scr_mode)
        return;
    plot(x, y, clamp_color(color));
    flip();
}

static void hspan(int x0, int x1, int y, int c) {
    int x;
    if (x0 > x1) { x = x0; x0 = x1; x1 = x; }
    for (x = x0; x <= x1; x++)
        plot(x, y, c);
}

/* LINE's style, for the one LINE: a mask of 16 pixels, carried along it and
 * round a box's sides */
static unsigned line_mask = 0xFFFF;
static unsigned line_at;

void sb_gfx_line_style(int mask) {
    line_mask = (unsigned)mask & 0xFFFF;
}

/* n * d, by the line's slope: a line cut at an edge, rounded half away from
 * zero */
static long cut_by(long n, long d, long den) {
    long long a = (long long)n * d, b = den, r;
    int neg = (a < 0) != (b < 0);
    if (a < 0) a = -a;
    if (b < 0) b = -b;
    r = (2 * a + b) / (2 * b);
    return (long)(neg ? -r : r);
}

enum { CUT_L = 1, CUT_R = 2, CUT_T = 4, CUT_B = 8 };

static int out_code(int x, int y) {
    return (x < vx1 ? CUT_L : 0) | (x > vx2 ? CUT_R : 0) | (y < vy1 ? CUT_T : 0) | (y > vy2 ? CUT_B : 0);
}

/* a line clipped to the viewport as QBasic clips one before it draws it
 * (fitted to 1800 of its lines): the end named first, then the other, each
 * at the left, right, top and bottom in turn, from where that end has got
 * to; 0 when nothing is left.  The pixels follow from the cut ends, so a
 * line that runs off the edge has its slope changed a little. */
static int clip_line(int *x0, int *y0, int *x1, int *y1) {
    int *ex[2] = { x0, x1 }, *ey[2] = { y0, y1 };
    for (int end = 0; end < 2; end++) {
        int *ax = ex[end], *ay = ey[end], *bx = ex[1 - end], *by = ey[1 - end];
        for (int bit = CUT_L; bit <= CUT_B; bit <<= 1) {
            if (out_code(*ax, *ay) & out_code(*bx, *by)) return 0;
            if (!(out_code(*ax, *ay) & bit)) continue;
            if (bit == CUT_L || bit == CUT_R) {
                int edge = bit == CUT_L ? vx1 : vx2;
                *ay += (int)cut_by(*by - *ay, edge - *ax, *bx - *ax);
                *ax = edge;
            } else {
                int edge = bit == CUT_T ? vy1 : vy2;
                *ax += (int)cut_by(*bx - *ax, edge - *ay, *by - *ay);
                *ay = edge;
            }
        }
    }
    return !(out_code(*x0, *y0) | out_code(*x1, *y1));
}

/* LINE as QBasic draws one (fitted to its pixels, SCREEN 13, every slope
 * to 14 by 14): always left to right, whichever end was named first; along
 * the longer axis one step at a time, the other stepping when 3M + 4im has
 * reached 4Mj, M and m the two lengths: the error starts at three
 * quarters, not at a half. */
static void draw_line(int x0, int y0, int x1, int y1, int c) {
    int dx, dy, sy, major, minor, e, x, y;
    if (!clip_line(&x0, &y0, &x1, &y1)) return;
    if (x0 > x1) {
        x = x0; x0 = x1; x1 = x;
        y = y0; y0 = y1; y1 = y;
    }
    dx = x1 - x0;
    dy = y1 > y0 ? y1 - y0 : y0 - y1;
    sy = y0 < y1 ? 1 : -1;
    major = dx >= dy ? dx : dy;
    minor = dx >= dy ? dy : dx;
    e = 3 * major;
    x = x0;
    y = y0;
    for (int i = 0; i <= major; i++) {
        if (line_mask == 0xFFFF || (line_mask >> (15 - line_at % 16) & 1)) plot(x, y, c);
        line_at++;
        e += 4 * minor;
        if (dx >= dy) x++; else y += sy;
        if (e >= 4 * major) {
            e -= 4 * major;
            if (dx >= dy) y += sy; else x++;
        }
    }
}

void sb_gfx_line(int x0, int y0, int x1, int y1, int color, int box) {
    int c, y;
    if (!scr_mode)
        return;
    c = clamp_color(color);
    line_at = 0;
    if (box == 0) {
        draw_line(x0, y0, x1, y1, c);
    } else if (box == 1 && line_mask != 0xFFFF) {
        /* (the style's way round, fitted: the row at the second y, the row at
         * the first, each from the left; then the column at the second x and
         * the one at the first, each first y to second) */
        int lo = x0 < x1 ? x0 : x1, hi = x0 < x1 ? x1 : x0;
        draw_line(lo, y1, hi, y1, c);
        draw_line(lo, y0, hi, y0, c);
        draw_line(x1, y0, x1, y1, c);
        draw_line(x0, y0, x0, y1, c);
    } else if (box == 1) {
        hspan(x0, x1, y0, c);
        hspan(x0, x1, y1, c);
        draw_line(x0, y0, x0, y1, c);
        draw_line(x1, y0, x1, y1, c);
    } else {
        int ylo = y0 < y1 ? y0 : y1;
        int yhi = y0 < y1 ? y1 : y0;
        for (y = ylo; y <= yhi; y++)
            hspan(x0, x1, y, c);
    }
    line_mask = 0xFFFF;
    flip();
}

/* CIRCLE, as QBasic draws one (fitted to its pixels, SCREEN 7): the
 * midpoint circle of radius r, one axis scaled by the aspect and
 * rounded -- y by it when it is under 1, x by its inverse over 1.  An
 * arc keeps the points from start to end counterclockwise, each end
 * tested within its octant by the coordinate the octant steps (so the
 * end's own rounded pixel is in); a negative angle's end is joined to
 * the center. */

/* a point's octant (math axes, y up), 0..7 counterclockwise */
static int octant_of(int x, int y) {
    double t = atan2((double)y, (double)x);
    int k;
    if (t < 0) t += 2 * M_PI;
    k = (int)(t / (M_PI / 4));
    return k > 7 ? 7 : k;
}

/* the coordinate octant k steps, growing with the angle */
static int octant_step(int k, int x, int y) {
    switch (k) {
        case 0: case 7: return y;
        case 1: case 2: return -x;
        case 3: case 4: return -y;
        default:        return x;
    }
}

static int angle_octant(double t) {
    int k = (int)(t / (M_PI / 4));
    return k > 7 ? 7 : k;
}

static int on_arc(int x, int y, double s, double e, int sx, int sy, int ex, int ey) {
    int k = octant_of(x, y), sk = angle_octant(s), ek = angle_octant(e);
    int after = k != sk || octant_step(k, x, y) >= octant_step(k, sx, sy);
    int before = k != ek || octant_step(k, x, y) <= octant_step(k, ex, ey);
    if (k == 0 && y == 0 && x > 0 && e >= 2 * M_PI - 1e-9)
        return 1;                       /* (an arc to 2pi ends where it began: 0) */
    if (sk < ek || (sk == ek && s <= e)) return k >= sk && k <= ek && after && before;
    if (sk == ek) return after || before;           /* (the arc round the long way) */
    return (k == sk ? after : k > sk) || (k == ek ? before : k < ek);
}

/* rounded as CINT rounds, half to even (QBasic's ellipses are symmetric) */
static int scaled(int v, double f) { return (int)rint(v * f); }

void sb_gfx_circle(int cx, int cy, int r, int color, double aspect,
                   int arc, double s, double e, int sline, int eline) {
    int c, x, y, err, sx = 0, sy = 0, ex = 0, ey = 0;
    double fx = aspect > 1 ? 1 / aspect : 1, fy = aspect > 1 ? 1 : aspect;
    if (!scr_mode)
        return;
    c = clamp_color(color);
    if (r < 0)
        r = -r;
    if (arc) {
        sx = scaled(1, r * cos(s)); sy = scaled(1, r * sin(s));
        ex = scaled(1, r * cos(e)); ey = scaled(1, r * sin(e));
    }
    x = r;
    y = 0;
    err = 1 - r;
    while (x >= y) {
        int pt[8][2] = { { x, y }, { -x, y }, { x, -y }, { -x, -y },
                         { y, x }, { -y, x }, { y, -x }, { -y, -x } };
        for (int i = 0; i < 8; i++)     /* (pt: math axes, y up) */
            if (!arc || on_arc(pt[i][0], pt[i][1], s, e, sx, sy, ex, ey))
                plot(cx + scaled(pt[i][0], fx), cy - scaled(pt[i][1], fy), c);
        y++;
        if (err < 0) {
            err += 2 * y + 1;
        } else {
            x--;
            err += 2 * (y - x) + 1;
        }
    }
    if (sline) draw_line(cx, cy, cx + scaled(sx, fx), cy - scaled(sy, fy), c);
    if (eline) draw_line(cx, cy, cx + scaled(ex, fx), cy - scaled(ey, fy), c);
    flip();
}

/* the mode's CIRCLE aspect when none is given: a 4:3 screen's pixels' */
double sb_gfx_aspect(void) {
    /* (a SINGLE of seven decimal digits, as if QBasic's were written out:
     * .8333333, a hair under 5/6, and .4166667, a hair over 5/12 -- which
     * decides ties) */
    if (!scr_mode) return 1.0;
    return (float)(floor(4.0 / 3.0 * scr_h / scr_w * 1e7 + 0.5) / 1e7);
}

/* PAINT, as QBasic fills (fitted to its pixels): a span runs from the seed
 * to the border, or the viewport's edge, either way, and only those stop it;
 * all of it is painted; and it spreads to the rows above and below only if a
 * pixel in it changed, or the paint's row at this y is the background's
 * pattern, whole (no background given: 0's) -- so a row already looking like
 * the paint ends the fill, and the first span spreads whatever it found.  A
 * solid color is a tile of one color. */
typedef struct { int x, y; } seed_t;

/* a tile's color at a pixel: the row by y on the screen, the mode's bytes
 * in it: a byte a pixel (13), the bits of 1 or 2 per pixel, or the planes'
 * bytes in turn; what the string does not reach is 0 */
int sb_gfx_tile_row(void) {
    switch (scr_mode) {
        case 7: case 8: case 9: case 12: return 4;
        case 10: return 2;
        default: return 1;
    }
}

static int tile_color(int x, int y, const unsigned char *t, int len) {
    int row = sb_gfx_tile_row(), rows = (len + row - 1) / row;
    int at = (y % rows) * row, c = 0;
    if (scr_mode == 13) return t[at];
    if (scr_mode == 1) return t[at] >> (6 - 2 * (x % 4)) & 3;
    for (int p = 0; p < row; p++)
        if (at + p < len && (t[at + p] >> (7 - x % 8) & 1)) c |= 1 << p;
    return c;
}

/* the tile's row at y is the background's pattern, whole */
static int tile_row_is(const unsigned char *t, int len, int y, const unsigned char *bg, int bglen) {
    int row = sb_gfx_tile_row(), rows = (len + row - 1) / row, at = (y % rows) * row;
    for (int p = 0; p < row; p++) {
        int a = at + p < len ? t[at + p] : 0, b = p < bglen ? bg[p] : 0;
        if (a != b) return 0;
    }
    return 1;
}

static int in_view(int x, int y) { return x >= vx1 && x <= vx2 && y >= vy1 && y <= vy2; }

static void paint_fill(int x, int y, int solid, const unsigned char *t, int len, int border,
                       const unsigned char *bg, int bglen) {
    seed_t *stack;
    uint8_t *seen;              /* (a background's row looks the same after: it counts the once) */
    int top = 0, cap = 1024, first = 1;
#define PAINT_BORDER(px, py) (pixels[(py) * scr_w + (px)] == (uint8_t)border)
    if (!in_view(x, y) || PAINT_BORDER(x, y)) { flip(); return; }
    stack = malloc((size_t)cap * sizeof(seed_t));
    if (!stack) return;
    /* (no background given: 0's, a blank pixel not being painted yet) */
    if (!bg) {
        static const unsigned char zero[1] = { 0 };
        bg = zero;
        bglen = 1;
    }
    if (!(seen = calloc((size_t)scr_w * scr_h, 1))) { free(stack); return; }
    stack[0].x = x;
    stack[0].y = y;
    top = 1;
    while (top > 0) {
        int lx, rx, cy, changed = first, row_is_bg;
        top--;
        x = stack[top].x;
        cy = stack[top].y;
        if (!in_view(x, cy) || PAINT_BORDER(x, cy)) continue;
        row_is_bg = t ? tile_row_is(t, len, cy, bg, bglen) : solid == 0;
        lx = rx = x;
        while (lx - 1 >= vx1 && !PAINT_BORDER(lx - 1, cy)) lx--;
        while (rx + 1 <= vx2 && !PAINT_BORDER(rx + 1, cy)) rx++;
        for (int sx = lx; sx <= rx; sx++) {
            uint8_t *p = &pixels[cy * scr_w + sx];
            int c = t ? tile_color(sx, cy, t, len) : solid;
            if (*p != (uint8_t)c) changed = 1;
            else if (row_is_bg && !seen[cy * scr_w + sx]) changed = 1;
            *p = (uint8_t)c;
            seen[cy * scr_w + sx] = 1;
        }
        first = 0;
        if (!changed) continue;
        for (int row = cy - 1; row <= cy + 1; row += 2) {
            if (row < vy1 || row > vy2) continue;
            for (int sx = lx; sx <= rx; sx++) {
                if (PAINT_BORDER(sx, row) || (sx > lx && !PAINT_BORDER(sx - 1, row))) continue;
                if (top == cap) {
                    seed_t *ns;
                    cap *= 2;
                    ns = realloc(stack, (size_t)cap * sizeof(seed_t));
                    if (!ns) { free(stack); free(seen); flip(); return; }
                    stack = ns;
                }
                stack[top].x = sx;
                stack[top].y = row;
                top++;
            }
        }
    }
#undef PAINT_BORDER
    free(stack);
    free(seen);
    flip();
}

void sb_gfx_paint(int x, int y, int paint, int border) {
    if (!scr_mode)
        return;
    paint = clamp_color(paint);
    paint_fill(x, y, paint, NULL, 0, border < 0 ? paint : mode_color(border), NULL, 0);
}

void sb_gfx_paint_tile(int x, int y, const unsigned char *tile, int len, int border,
                       const unsigned char *bg, int bglen) {
    if (!scr_mode)
        return;
    paint_fill(x, y, 0, tile, len, border < 0 ? clamp_color(-1) : mode_color(border), bg, bglen);
}

void sb_gfx_cls(void) {
    if (!scr_mode)
        return;
    for (int y = vy1; y <= vy2; y++)    /* (the viewport: QBasic's CLS, 0 and 1) */
        memset(pixels + y * scr_w + vx1, 0, (size_t)(vx2 - vx1 + 1));
    flip();
}

/* PALETTE attr, value (attr < 0: PALETTE alone, the default back), as
 * QBasic takes them (probed): an attribute the mode has (in 13 its low
 * byte); a value of -1 leaves it; in the EGA modes the value is a color
 * for the attribute's palette register -- one of the 16 at 200 lines
 * (1, 2, 7, 8), any of the 64 in 9; at VGA
 * (11, 12, 13) it is blue * 65536 + green * 256 + red, six bits each,
 * the attribute's DAC entry.  -1 when QBasic says Illegal function call. */
int sb_gfx_palette(int attr, int value) {
    if (!scr_mode)
        return 0;
    if (attr < 0) {
        build_default_palette();
        flip();
        return 0;
    }
    if (scr_colors == 256) attr &= 255;
    if (attr >= scr_colors) return -1;
    if (value == -1) return 0;
    if (scr_mode <= 9) {
        if (value < 0 || value > (scr_mode == 9 ? 63 : 15)) return -1;
        attr_map[attr] = scr_mode == 9 ? (uint8_t)value : map16[value];
        palette_sync();
        flip();
        return 0;
    }
    if (value < 0 || (value & 0xC0C0C0) || value > 0x3F3F3F) return -1;
    sb_gfx_dac_set(scr_colors == 256 ? attr : attr_map[attr], value & 63, value >> 8 & 63, value >> 16 & 63);
    return 0;
}

/* --- DRAW ----------------------------------------------------------- */

/* DRAW's state between statements (QuickBASIC's): its color, its scale
 * (S: a quarter pixel each), its angle in degrees (A: quarter turns, TA:
 * any) */
static int draw_color = -1, draw_scale = 4;
static double draw_angle;

static void draw_reset(void) { draw_color = -1; draw_scale = 4; draw_angle = 0; }

/* a number at *q: its digits, a sign first if may_sign; *n it (none: 1,
 * *given 0) */
static int draw_num(const char **q, const char *end, int may_sign, long *n, int *given) {
    const char *p = *q;
    int neg = 0, any = 0;
    long v = 0;
    while (p < end && (*p == ' ' || *p == '\t')) p++;
    if (p < end && *p == '=') {         /* = and a VARPTR$ of a number */
        if (end - p < 4 || !pc_var_number((unsigned char)p[1], (unsigned char)p[2] | (unsigned char)p[3] << 8, n))
            return 0;
        *q = p + 4;
        *given = 1;
        return 1;
    }
    if (may_sign && p < end && (*p == '+' || *p == '-')) neg = *p++ == '-';
    while (p < end && *p >= '0' && *p <= '9') { v = v * 10 + (*p++ - '0'); any = 1; if (v > 100000) return 0; }
    *q = p;
    *given = any;
    *n = any ? (neg ? -v : v) : 1;
    return any || !neg;
}

int sb_gfx_draw(const char *s, int len) {
    const char *q = s, *end = s + len;
    double px, py, aspect = sb_gfx_aspect();
    int ox = org_x(), oy = org_y();
    if (!scr_mode) return 0;
    /* (the view's pixels, fractional: QuickBASIC's turns leave a fraction) */
    px = to_view(0, last_lx);
    py = to_view(1, last_ly);
    while (q < end) {
        int c = *q++, nodraw = 0, back = 0, given, ix, iy, jx, jy;
        long n;
        double dx = 0, dy = 0;
        if (c == ' ' || c == '\t' || c == ';') continue;
        if (c >= 'a' && c <= 'z') c -= 32;
        while (c == 'B' || c == 'N') {          /* move without drawing; return after */
            if (c == 'B') nodraw = 1; else back = 1;
            while (q < end && (*q == ' ' || *q == '\t')) q++;
            if (q >= end) return -1;
            c = *q++;
            if (c >= 'a' && c <= 'z') c -= 32;
        }
        switch (c) {
            case 'U': case 'D': case 'L': case 'R': case 'E': case 'F': case 'G': case 'H': {
                static const char dirs[] = "UDLREFGH";
                static const int vx[] = { 0, 0, -1, 1, 1, 1, -1, -1 }, vy[] = { -1, 1, 0, 0, -1, 1, 1, -1 };
                int k = (int)(strchr(dirs, c) - dirs);
                if (!draw_num(&q, end, 0, &n, &given)) return -1;
                n = n * draw_scale / 4;            /* (whole pixels: QuickBASIC's S truncates) */
                dx = vx[k] * (double)n;
                dy = vy[k] * (double)n;
                break;
            }
            case 'M': {
                long y;
                const char *p0 = q;
                while (p0 < end && (*p0 == ' ' || *p0 == '\t')) p0++;
                int rel = p0 < end && (*p0 == '+' || *p0 == '-');
                if (!draw_num(&q, end, 1, &n, &given) || !given) return -1;
                while (q < end && (*q == ' ' || *q == '\t')) q++;
                if (q >= end || *q != ',') return -1;
                q++;
                if (!draw_num(&q, end, 1, &y, &given) || !given) return -1;
                if (!rel) {                         /* M x,y: the point itself */
                    double nx = (double)n - ox, ny = (double)y - oy;
                    if (!nodraw)
                        draw_line((int)rint(px) + ox, (int)rint(py) + oy, (int)n, (int)y, clamp_color(draw_color));
                    if (!back) { px = nx; py = ny; }
                    continue;
                }
                dx = (double)(n * draw_scale / 4);
                dy = (double)(y * draw_scale / 4);
                break;
            }
            case 'C':
                if (!draw_num(&q, end, 0, &n, &given) || !given) return -1;
                draw_color = (int)n;
                continue;
            case 'S':
                if (!draw_num(&q, end, 0, &n, &given) || !given || n > 255) return -1;
                draw_scale = n ? (int)n : 4;
                continue;
            case 'A':
                if (q < end && (*q == 'T' || *q == 't')) {      /* TA n: degrees, -360..360 */
                    q++;
                    if (!draw_num(&q, end, 1, &n, &given) || !given || n < -360 || n > 360) return -1;
                    draw_angle = (double)n;
                } else {
                    if (!draw_num(&q, end, 0, &n, &given) || !given || n > 3) return -1;
                    draw_angle = 90.0 * (double)n;
                }
                continue;
            case 'T':                               /* (TA, its T first) */
                if (q >= end || (*q != 'A' && *q != 'a')) return -1;
                q++;
                if (!draw_num(&q, end, 1, &n, &given) || !given || n < -360 || n > 360) return -1;
                draw_angle = (double)n;
                continue;
            case 'P': {
                long b;
                if (!draw_num(&q, end, 0, &n, &given) || !given) return -1;
                while (q < end && (*q == ' ' || *q == '\t')) q++;
                if (q >= end || *q != ',') return -1;
                q++;
                if (!draw_num(&q, end, 0, &b, &given) || !given) return -1;
                sb_gfx_paint((int)rint(px) + ox, (int)rint(py) + oy, (int)n, (int)b);
                continue;
            }
            case 'X': {                             /* the string a VARPTR$ names, drawn here */
                const char *sub;
                int sublen;
                if (end - q < 3 || (unsigned char)q[0] != 3 ||
                    !pc_string_at((unsigned char)q[1] | (unsigned char)q[2] << 8, &sub, &sublen))
                    return -1;
                q += 3;
                last_lx = to_world(0, px);
                last_ly = to_world(1, py);
                if (sb_gfx_draw(sub, sublen) < 0) return -1;
                px = to_view(0, last_lx);
                py = to_view(1, last_ly);
                continue;
            }
            default:
                return -1;
        }
        if (draw_angle != 0) {                      /* turned where a pixel is square */
            double t = draw_angle * M_PI / 180, mx = dx, my = -dy / aspect;
            double rx = mx * cos(t) - my * sin(t), ry = mx * sin(t) + my * cos(t);
            dx = rx;
            dy = -ry * aspect;
        }
        ix = (int)rint(px); iy = (int)rint(py);
        jx = (int)rint(px + dx); jy = (int)rint(py + dy);
        if (!nodraw) draw_line(ix + ox, iy + oy, jx + ox, jy + oy, clamp_color(draw_color));
        if (!back) { px += dx; py += dy; }
    }
    last_lx = to_world(0, px);
    last_ly = to_world(1, py);
    flip();
    return 0;
}

/* --- GET and PUT's images ------------------------------------------- */

/* the mode's layout: bits a pixel has in a plane, and planes */
static void image_layout(int *bits, int *planes) {
    switch (scr_mode) {
        case 13: *bits = 8; *planes = 1; break;
        case 1:  *bits = 2; *planes = 1; break;
        case 2: case 11: *bits = 1; *planes = 1; break;
        default: *bits = 1; *planes = 4; break;     /* 7, 8, 9, 12 */
    }
}

static int image_row(int w, int bits) { return (w * bits + 7) / 8; }

int sb_gfx_image_size(int w, int h) {
    int bits, planes;
    if (!scr_mode) return 0;
    image_layout(&bits, &planes);
    return 4 + image_row(w, bits) * planes * h;
}

void sb_gfx_get(int x1, int y1, int x2, int y2, unsigned char *buf) {
    int bits, planes, w = x2 - x1 + 1, h = y2 - y1 + 1, row, mask;
    image_layout(&bits, &planes);
    row = image_row(w, bits);
    mask = (1 << bits) - 1;
    buf[0] = (unsigned char)(w * bits);
    buf[1] = (unsigned char)((w * bits) >> 8);
    buf[2] = (unsigned char)h;
    buf[3] = (unsigned char)(h >> 8);
    memset(buf + 4, 0, (size_t)row * planes * h);
    for (int y = 0; y < h; y++)
        for (int x = 0; x < w; x++) {
            int sx = x1 + x, sy = y1 + y, c;
            if (sx < 0 || sx >= scr_w || sy < 0 || sy >= scr_h) continue;
            c = pixels[sy * scr_w + sx];
            for (int p = 0; p < planes; p++) {
                int v = planes > 1 ? (c >> p) & 1 : c & mask;
                int bit = x * bits;
                unsigned char *q = buf + 4 + (y * planes + p) * row + bit / 8;
                *q |= (unsigned char)(v << (8 - bits - bit % 8));
            }
        }
}

int sb_gfx_put(int x, int y, const unsigned char *buf, int len, int action) {
    int bits, planes, w, h, row, mask, all;
    if (!scr_mode || len < 4) return -1;
    image_layout(&bits, &planes);
    w = (short)(buf[0] | buf[1] << 8) / bits;
    h = (short)(buf[2] | buf[3] << 8);
    row = image_row(w, bits);
    if (w <= 0 || h <= 0 || 4 + row * planes * h > len) return -1;
    if (x < 0 || y < 0 || x + w > scr_w || y + h > scr_h) return -1;
    mask = (1 << bits) - 1;
    all = planes > 1 ? 15 : mask;               /* a pixel's colors */
    for (int yy = 0; yy < h; yy++)
        for (int xx = 0; xx < w; xx++) {
            int c = 0, bit = xx * bits, old, nv;
            for (int p = 0; p < planes; p++) {
                const unsigned char *q = buf + 4 + (yy * planes + p) * row + bit / 8;
                int v = (*q >> (8 - bits - bit % 8)) & mask;
                c |= planes > 1 ? v << p : v;
            }
            old = pixels[(y + yy) * scr_w + x + xx];
            switch (action) {
                case 1:  nv = c; break;
                case 2:  nv = ~c & all; break;
                case 3:  nv = old & c; break;
                case 4:  nv = old | c; break;
                default: nv = old ^ c; break;
            }
            pixels[(y + yy) * scr_w + x + xx] = (uint8_t)nv;
        }
    flip();
    return 0;
}

int sb_gfx_mode(void) { return scr_mode; }

void sb_gfx_set_fg(int color) { fg = mode_color(color); }

int sb_gfx_fb_get(int off) {
    if (scr_mode != 13 || off < 0 || off >= scr_w * scr_h) return -1;
    return pixels[off];
}

void sb_gfx_fb_set(int off, int v) {
    if (scr_mode != 13 || off < 0 || off >= scr_w * scr_h) return;
    pixels[off] = (uint8_t)v;
    flip();
}

int sb_gfx_point(int x, int y) {
    if (!scr_mode || x < vx1 || x > vx2 || y < vy1 || y > vy2)
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
