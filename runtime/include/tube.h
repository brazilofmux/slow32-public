#ifndef _TUBE_H
#define _TUBE_H

#include <stdint.h>

/* Guest API for the negotiated tube service. See docs/TUBE.md. */

#define TUBE_MODE_VEC 1
#define TUBE_MODE_FB  2
#define TUBE_MODE_PPU 3

#define TUBE_OP_END   0
#define TUBE_OP_MOVE  1
#define TUBE_OP_DRAW  2
#define TUBE_OP_POINT 3
#define TUBE_OP_INTEN 4
#define TUBE_OP_COLOR 5

#define TUBE_WORD(op, x, y) \
    ((((uint32_t)(op) & 0xFu) << 28) | \
     (((uint32_t)(x) & 0xFFFu) << 16) | \
     (((uint32_t)(y) & 0xFFFu) << 4))

#define TUBE_WORD_INTEN(i) \
    ((TUBE_OP_INTEN << 28) | ((uint32_t)(i) & 0xFFu))

#define TUBE_WORD_COLOR(rgb) \
    ((TUBE_OP_COLOR << 28) | ((uint32_t)(rgb) & 0xFFFFFFu))

/* Negotiate the service. Returns 0 on success, -1 if denied/unavailable. */
int  tube_init(void);
void tube_cleanup(void);

/* Open a mode (vec takes no parameters). Returns 0 or -1. */
int  tube_open(uint32_t mode);
int  tube_close(void);

/* Open the framebuffer: 8bpp indexed pixels (w*h bytes, row-major,
   top-left origin) plus a 256 x u32 0x00RRGGBB palette. The host
   re-reads both on every flip. */
int  tube_open_fb(uint32_t w, uint32_t h, const void *pixels,
                  const uint32_t *palette);

/* Open the PPU: regs is 16 x u32 in guest RAM (see docs/TUBE.md #5):
   pattern_base, nametable_base, oam_base, palette_base, nt_w, nt_h,
   scroll_x, scroll_y, bg_color, flags, reserved... */
int  tube_open_ppu(const uint32_t *regs);

/* Present a display list in guest RAM (vec mode). length is a word
   count. Returns 0 or -1. */
int  tube_present(const uint32_t *list, uint32_t words, uint32_t generation);

/* Present the current fb/ppu state (a snapshot barrier). */
int  tube_flip(uint32_t generation);

/* Packed INFO / STATUS words; 0 if the service is not granted. */
uint32_t tube_info(void);
uint32_t tube_status(void);

/* Copy up to nbytes (multiple of 4) key events. Returns event count. */
int  tube_keys(void *buf, uint32_t nbytes);

#endif /* _TUBE_H */
