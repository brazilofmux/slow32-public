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

/* Open a mode (TUBE_MODE_VEC in v1). Returns 0 or -1. */
int  tube_open(uint32_t mode);
int  tube_close(void);

/* Present a display list in guest RAM. length is a word count.
   Returns 0 or -1. */
int  tube_present(const uint32_t *list, uint32_t words, uint32_t generation);

/* Packed INFO / STATUS words; 0 if the service is not granted. */
uint32_t tube_info(void);
uint32_t tube_status(void);

/* Copy up to nbytes (multiple of 4) key events. Returns event count. */
int  tube_keys(void *buf, uint32_t nbytes);

#endif /* _TUBE_H */
