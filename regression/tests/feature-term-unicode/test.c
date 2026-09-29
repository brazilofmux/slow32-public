/* The term service is Unicode-aware (common/s32utf.h, mmio_ring.c): a screen
 * cell holds a grapheme cluster and a character takes its display width.
 *   - a buffered update's diff (the repaint) writes whole UTF-8 characters,
 *     and overwriting the first half of a double-width character blanks
 *     its second half;
 *   - a combining mark joins the cell before it, so a saved and restored
 *     screen paints it with its base;
 *   - term_getchar() reads one character, not one byte: U+00E9, U+65E5,
 *     then U+FFFD for a byte that is not UTF-8, then -1 at end of input. */
#include <stdio.h>
#include "term.h"

static void hex(int v)
{
    char b[16]; int n = 0;
    if (v < 0) { term_puts(" EOF"); return; }
    b[n++] = ' '; b[n++] = 'U'; b[n++] = '+';
    for (int s = 12; s >= 0; s -= 4) b[n++] = "0123456789ABCDEF"[(v >> s) & 15];
    b[n] = 0;
    term_puts(b);
}

int main(void)
{
    if (term_init() != 0) { puts("no term service"); return 1; }
    term_clear(0);
    term_puts("A\xE6\x97\xA5\xC3\xA9|");                /* A 日 é | : columns 1, 2-3, 4, 5 */
    term_begin_update();
    term_gotoxy(1, 1); term_puts("x");                  /* over A */
    term_gotoxy(1, 2); term_puts("Y");                  /* over the first half of 日: the second goes blank */
    term_gotoxy(2, 1); term_puts("e\xCC\x81!");         /* e + U+0301 in one cell, then ! */
    term_end_update();
    term_save_screen();
    term_clear(0);
    term_restore_screen();                              /* the repaint: x Y _ é | / é ! */
    term_gotoxy(3, 1);
    term_puts("keys:");
    for (int i = 0; i < 4; i++) hex(term_getchar());
    term_puts("\n");
    term_cleanup();
    return 0;
}
