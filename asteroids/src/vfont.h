#ifndef VFONT_H
#define VFONT_H

#include <stdint.h>

/* Stroke font for the vec tube. Glyphs live on a 6x10 grid; `size` is
 * the pixel width of one grid unit, so a glyph is 6*size wide and
 * 10*size tall, advance 8*size. Emits DRAW/MOVE words into `list` at
 * *n. Coordinates are clamped to the 0..4095 tube space. */
void vfont_text(uint32_t *list, uint32_t *n, int x, int y, int size,
                const char *s);
void vfont_uint(uint32_t *list, uint32_t *n, int x, int y, int size,
                unsigned v, int min_digits);

#endif /* VFONT_H */
