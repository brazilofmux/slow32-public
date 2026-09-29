/* The term service's Unicode model is common/s32utf.h (cobol ISSUES-94):
 *   - a cell holds a whole grapheme cluster (UAX #29): a family emoji of
 *     five code points is one wide cell, and the character after it is not
 *     swallowed into it; "a ZWJ b" is two clusters (GB11 joins only emoji);
 *   - a flag (two regional indicators) and a heart with U+FE0F are two
 *     columns, so what follows them lands in column 3;
 *   - what is not UTF-8 -- an overlong NUL, an encoded surrogate, a code
 *     point past U+10FFFF -- is U+FFFD per maximal subpart, never raw bytes;
 *   - term_getchar() decodes the same way and gives back the byte that cut
 *     a sequence short: C3 41 is U+FFFD then U+0041, and a sequence cut
 *     off by the end of input is one U+FFFD.
 * Everything is painted inside begin/end_update, so the output is the
 * shadow's repaint of its cells.  A second update writes '#' into column
 * 2 of rows 1-4: where a wide cluster holds columns 1-2 the repaint blanks
 * its first half ("ESC[r;1H #"); row 2's column 2 is the narrow 'b'. */
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
    if (term_init() != 0) { puts("no term"); return 1; }
    term_clear(0);
    term_begin_update();
    term_gotoxy(1, 1); term_puts("\xF0\x9F\x91\xA8\xE2\x80\x8D\xF0\x9F\x91\xA9\xE2\x80\x8D\xF0\x9F\x91\xA7x|");
    term_gotoxy(2, 1); term_puts("a\xE2\x80\x8D" "b|");
    term_gotoxy(3, 1); term_puts("\xF0\x9F\x87\xBA\xF0\x9F\x87\xB8|");
    term_gotoxy(4, 1); term_puts("\xE2\x9D\xA4\xEF\xB8\x8F|");
    term_gotoxy(5, 1); term_puts("[\xE0\x80\x80][\xED\xA0\x80][\xF4\x90\x80\x80]");
    term_end_update();
    term_begin_update();
    for (int r = 1; r <= 4; r++) { term_gotoxy(r, 2); term_puts("#"); }
    term_end_update();
    term_gotoxy(7, 1);
    int c;
    while ((c = term_getchar()) >= 0) hex(c);
    hex(c);
    term_puts("\n");
    term_cleanup();
    return 0;
}
