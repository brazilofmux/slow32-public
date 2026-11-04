#ifndef SBASIC_GRAPHICS_H
#define SBASIC_GRAPHICS_H

/* GW-BASIC graphics statements as a guest-side layer over the tube's
 * fb mode (see docs/TUBE.md #4). Statements only, no fake hardware
 * registers; with no tube granted, SCREEN prints a one-time message
 * and every drawing statement degrades to a no-op, the way BROWSE
 * degrades to LIST. */

/* SCREEN mode. Returns 0 on success, -1 if the tube is denied or the
 * open failed (degraded: message printed once, drawing no-ops from
 * here on), -2 for an unknown mode number. Mode 0 closes the screen
 * and always succeeds. */
int sb_gfx_screen(int mode);

/* 1 while a graphics screen is open. */
int sb_gfx_active(void);

/* 1 once a SCREEN attempt has hit a denied/unavailable tube. */
int sb_gfx_degraded(void);

/* Screen size of the open mode (undefined when inactive). */
int sb_gfx_width(void);
int sb_gfx_height(void);

/* Drawing. color -1 means "current foreground". All coordinates are
 * clipped, never errors. Every call presents the frame (immediate
 * mode, the GW-BASIC feel). */
void sb_gfx_pset(int x, int y, int color);          /* updates last point */
void sb_gfx_line(int x0, int y0, int x1, int y1, int color,
                 int box);                          /* box: 0=line 1=B 2=BF */
void sb_gfx_circle(int x, int y, int r, int color);
void sb_gfx_paint(int x, int y, int paint, int border);
void sb_gfx_cls(void);

/* PALETTE attr, rgb24 (0xRRGGBB). attr -1 restores the default
 * palette. The host re-reads the palette every present, so this
 * animates already-drawn pixels. */
void sb_gfx_palette(int attr, int rgb);

/* GET and PUT's images, laid out as QuickBASIC lays them out for the
 * mode: two 16-bit words, the width times the bits a pixel has in one
 * plane, and the height; then the rows, each a plane after another
 * (EGA/VGA 7, 8, 9, 12: four planes of a bit; CGA 1: one of two bits;
 * 2: one of one; 13: one of a byte), a plane's row a whole number of
 * bytes, the leftmost pixel in the high bits. */

/* the bytes an image of w by h pixels takes (its header too), 0 with no screen */
int sb_gfx_image_size(int w, int h);

/* the rectangle x1..x2, y1..y2 (in order) into buf (sb_gfx_image_size
 * bytes): a pixel off the screen is 0 */
void sb_gfx_get(int x1, int y1, int x2, int y2, unsigned char *buf);

/* buf's image (len bytes there) drawn with its top left at x, y by
 * action (0 XOR, 1 PSET, 2 PRESET, 3 AND, 4 OR); -1 when it is not all
 * on the screen or len is short of it (QuickBASIC: Illegal function
 * call), else 0 */
int sb_gfx_put(int x, int y, const unsigned char *buf, int len, int action);

/* POINT(x, y): pixel value, or -1 (no screen / out of range). */
int sb_gfx_point(int x, int y);

/* Last referenced point, for STEP and for LINE -(x,y): the world's
 * (WINDOW's; without one, the viewport's pixel). */
void sb_gfx_last(double *x, double *y);
void sb_gfx_set_last(double x, double y);

/* VIEW [SCREEN] (x1, y1)-(x2, y2)[, fill[, border]] (on 0: VIEW alone;
 * fill, border -1 when left out); -1 for one off the screen or of no
 * size (Illegal function call) */
int sb_gfx_view(int on, int scr, int x1, int y1, int x2, int y2, int fill, int border);
/* WINDOW [SCREEN] (x1, y1)-(x2, y2) (on 0: WINDOW alone); -1 for no width or height */
int sb_gfx_window(int on, int scr, double x1, double y1, double x2, double y2);
/* a world point's pixel on the screen, rounded as CINT rounds; -1 when
 * it is past an int (Overflow) */
int sb_gfx_map(double x, double y, int *ax, int *ay);
int sb_gfx_windowed(void);
/* a world length's pixels across (CIRCLE's radius) */
double sb_gfx_xscale(void);
/* PMAP(v, n): n 0, 1 the world's x, y as the view's pixel; 2, 3 back */
int sb_gfx_pmap(double v, int n, double *out);
/* POINT(n): the last point, n 0, 1 the view's pixel, 2, 3 the world's */
int sb_gfx_point_n(int n, double *out);

/* Viewer key queue -> INKEY$: returns string length (1, or 2 with
 * out[0]=0 for extended scancodes, GW-style), 0 if no key. */
int sb_gfx_inkey(char out[3]);

#endif
