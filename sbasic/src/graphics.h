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

/* POINT(x, y): pixel value, or -1 (no screen / out of range). */
int sb_gfx_point(int x, int y);

/* Last referenced point, for STEP and for LINE -(x,y). */
void sb_gfx_last(int *x, int *y);

/* Viewer key queue -> INKEY$: returns string length (1, or 2 with
 * out[0]=0 for extended scancodes, GW-style), 0 if no key. */
int sb_gfx_inkey(char out[3]);

#endif
