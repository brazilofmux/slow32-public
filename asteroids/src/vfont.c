/* A DVG-style stroke font: the letters Asteroids actually needs, plus
 * digits. Each glyph is segments on a 6-wide, 10-tall grid. */

#include "vfont.h"

#include <tube.h>

typedef struct {
    char ch;
    uint8_t nseg;
    uint8_t seg[6][4]; /* x0 y0 x1 y1 */
} glyph_t;

static const glyph_t glyphs[] = {
    { '0', 4, { {0,0,6,0}, {6,0,6,10}, {6,10,0,10}, {0,10,0,0} } },
    { '1', 1, { {3,0,3,10} } },
    { '2', 5, { {0,10,6,10}, {6,10,6,5}, {6,5,0,5}, {0,5,0,0}, {0,0,6,0} } },
    { '3', 4, { {0,10,6,10}, {6,10,6,0}, {6,0,0,0}, {0,5,6,5} } },
    { '4', 3, { {0,10,0,5}, {0,5,6,5}, {6,10,6,0} } },
    { '5', 5, { {6,10,0,10}, {0,10,0,5}, {0,5,6,5}, {6,5,6,0}, {6,0,0,0} } },
    { '6', 5, { {6,10,0,10}, {0,10,0,0}, {0,0,6,0}, {6,0,6,5}, {6,5,0,5} } },
    { '7', 2, { {0,10,6,10}, {6,10,2,0} } },
    { '8', 5, { {0,0,6,0}, {6,0,6,10}, {6,10,0,10}, {0,10,0,0}, {0,5,6,5} } },
    { '9', 4, { {6,0,6,10}, {6,10,0,10}, {0,10,0,5}, {0,5,6,5} } },
    { 'A', 5, { {0,0,0,7}, {0,7,3,10}, {3,10,6,7}, {6,7,6,0}, {0,4,6,4} } },
    { 'C', 3, { {6,10,0,10}, {0,10,0,0}, {0,0,6,0} } },
    { 'D', 6, { {0,0,0,10}, {0,10,4,10}, {4,10,6,8}, {6,8,6,2}, {6,2,4,0}, {4,0,0,0} } },
    { 'E', 4, { {6,0,0,0}, {0,0,0,10}, {0,10,6,10}, {0,5,4,5} } },
    { 'G', 5, { {6,10,0,10}, {0,10,0,0}, {0,0,6,0}, {6,0,6,4}, {6,4,3,4} } },
    { 'H', 3, { {0,0,0,10}, {6,0,6,10}, {0,5,6,5} } },
    { 'I', 3, { {3,0,3,10}, {0,0,6,0}, {0,10,6,10} } },
    { 'M', 4, { {0,0,0,10}, {0,10,3,6}, {3,6,6,10}, {6,10,6,0} } },
    { 'O', 4, { {0,0,6,0}, {6,0,6,10}, {6,10,0,10}, {0,10,0,0} } },
    { 'P', 4, { {0,0,0,10}, {0,10,6,10}, {6,10,6,5}, {6,5,0,5} } },
    { 'R', 5, { {0,0,0,10}, {0,10,6,10}, {6,10,6,5}, {6,5,0,5}, {3,5,6,0} } },
    { 'S', 5, { {6,10,0,10}, {0,10,0,5}, {0,5,6,5}, {6,5,6,0}, {6,0,0,0} } },
    { 'T', 2, { {0,10,6,10}, {3,10,3,0} } },
    { 'U', 3, { {0,10,0,0}, {0,0,6,0}, {6,0,6,10} } },
    { 'V', 2, { {0,10,3,0}, {3,0,6,10} } },
};

static int clampc(int v) {
    if (v < 0) {
        return 0;
    }
    if (v > 4095) {
        return 4095;
    }
    return v;
}

static const glyph_t *find_glyph(char ch) {
    unsigned i;
    for (i = 0; i < sizeof(glyphs) / sizeof(glyphs[0]); i++) {
        if (glyphs[i].ch == ch) {
            return &glyphs[i];
        }
    }
    return 0;
}

void vfont_text(uint32_t *list, uint32_t *n, int x, int y, int size,
                const char *s) {
    for (; *s; s++, x += 8 * size) {
        const glyph_t *g = find_glyph(*s);
        int i;
        if (!g) {
            continue; /* space and unknowns advance silently */
        }
        for (i = 0; i < g->nseg; i++) {
            int x0 = clampc(x + g->seg[i][0] * size);
            int y0 = clampc(y + g->seg[i][1] * size);
            int x1 = clampc(x + g->seg[i][2] * size);
            int y1 = clampc(y + g->seg[i][3] * size);
            list[(*n)++] = TUBE_WORD(TUBE_OP_MOVE, x0, y0);
            list[(*n)++] = TUBE_WORD(TUBE_OP_DRAW, x1, y1);
        }
    }
}

void vfont_uint(uint32_t *list, uint32_t *n, int x, int y, int size,
                unsigned v, int min_digits) {
    char buf[12];
    int i = (int)sizeof(buf) - 1;
    buf[i] = '\0';
    do {
        buf[--i] = (char)('0' + v % 10u);
        v /= 10u;
        min_digits--;
    } while ((v || min_digits > 0) && i > 0);
    vfont_text(list, n, x, y, size, buf + i);
}
