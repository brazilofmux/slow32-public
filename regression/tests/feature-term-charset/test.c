/* The term service's charset (docs/SPEC.md 8.14, sub-opcode 18, and
 * 8.14.7): with CP437, each byte the program writes is one CP437
 * character -- shown as its UTF-8, held in the shadow as its code point,
 * so a repaint paints it right -- and each key reads back as its CP437
 * byte, '?' for one CP437 lacks; a charset that is neither 0 nor 437 is
 * refused and changes nothing; back to UTF-8, bytes are bytes again. */
#include <stdio.h>
#include "term.h"

static void hex(int v)
{
    char b[8]; int n = 0;
    if (v < 0) { term_puts(" EOF"); return; }
    b[n++] = ' ';
    b[n++] = "0123456789ABCDEF"[(v >> 4) & 15];
    b[n++] = "0123456789ABCDEF"[v & 15];
    b[n] = 0;
    term_puts(b);
}

int main(void)
{
    if (term_init() != 0) { puts("no term service"); return 1; }
    int r437 = term_set_charset(TERM_CHARSET_CP437);
    int rbad = term_set_charset(850);                   /* refused: still CP437 */
    term_clear(0);
    term_puts("\xDA\xC4\xC4\xBF caf\x82");             /* a box corner, rules, corner; cafe with e-acute */
    term_putc(0xFE);                                    /* a small square */
    term_begin_update();
    term_gotoxy(1, 2); term_puts("\xCD");               /* a double rule over the first single one */
    term_end_update();                                  /* the repaint writes it as UTF-8 */
    term_save_screen();
    term_clear(0);
    term_restore_screen();
    term_gotoxy(2, 1);
    term_puts("keys:");
    for (int i = 0; i < 4; i++) {                       /* a, e-acute 82, the CJK sun ?, a rule C4 */
        int k = term_getkey();
        hex(k);
    }
    int r0 = term_set_charset(TERM_CHARSET_UTF8);
    for (int i = 0; i < 3; i++) hex(term_getkey());     /* bytes again: C3 A9, then end of input */
    term_puts("\n");
    term_cleanup();
    printf("set 437 %d, set 850 %d, set 0 %d\n", r437, rbad, r0);
    return 0;
}
