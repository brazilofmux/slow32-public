#include "graphics.h"
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
    view_reset();
    win_on = 0;
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

static int clamp_color(int color) {
    if (color < 0)
        return fg;
    return color & (scr_colors - 1);
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
    if (sline) bresenham(cx, cy, cx + scaled(sx, fx), cy - scaled(sy, fy), c);
    if (eline) bresenham(cx, cy, cx + scaled(ex, fx), cy - scaled(ey, fy), c);
    flip();
}

/* the mode's CIRCLE aspect when none is given: a 4:3 screen's pixels' */
double sb_gfx_aspect(void) {
    return scr_mode ? (float)(4.0 / 3.0 * scr_h / scr_w) : 1.0;   /* (a SINGLE, as QBasic's) */
}

/* PAINT: seed fill bounded by the border color. Painted pixels stop
 * the walk too (except when paint == border, where the paint itself
 * is the boundary), so the fill terminates. Span-based seed stack. */
typedef struct { int x, y; } seed_t;

static int fillable(int x, int y, int paint, int border) {
    uint8_t p;
    if (x < vx1 || x > vx2 || y < vy1 || y > vy2)
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
    for (int y = vy1; y <= vy2; y++)    /* (the viewport: QBasic's CLS, 0 and 1) */
        memset(pixels + y * scr_w + vx1, 0, (size_t)(vx2 - vx1 + 1));
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

/* --- GET and PUT's images ------------------------------------------- */

/* the mode's layout: bits a pixel has in a plane, and planes */
static void image_layout(int *bits, int *planes) {
    switch (scr_mode) {
        case 13: *bits = 8; *planes = 1; break;
        case 1:  *bits = 2; *planes = 1; break;
        case 2:  *bits = 1; *planes = 1; break;
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
