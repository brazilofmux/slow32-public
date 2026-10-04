/* The term service's cursor style (docs/SPEC.md 8.14, sub-opcode 15, and
 * 8.14.5): each style's bytes are emitted at once -- inside a buffered
 * update too, where everything else waits -- a style that is not one of
 * the five is refused and changes nothing, save and restore leave it
 * alone, and releasing the service with any style but the terminal's own
 * puts that back. */
#include <stdio.h>
#include "term.h"

int main(void)
{
    if (term_init() != 0) { puts("no term service"); return 1; }
    term_clear(0);
    term_puts("a");
    int r4 = term_set_cursor(TERM_CURSOR_BAR);
    term_puts("b");
    term_begin_update();
    term_gotoxy(2, 1); term_puts("held");
    int r0 = term_set_cursor(TERM_CURSOR_HIDDEN);      /* goes out now; "held" at the end of the update */
    term_end_update();
    term_save_screen();
    int r2 = term_set_cursor(TERM_CURSOR_BLOCK);
    term_restore_screen();                             /* the screen, not the cursor's style */
    int r3 = term_set_cursor(TERM_CURSOR_UNDERLINE);
    int r1 = term_set_cursor(TERM_CURSOR_DEFAULT);
    int bad = term_set_cursor(5);                      /* refused: nothing emitted */
    int neg = term_set_cursor(-1);
    term_gotoxy(3, 1);
    char b[64];
    snprintf(b, sizeof b, "results %d %d %d %d %d bad %d %d", r0, r1, r2, r3, r4, bad, neg);
    term_puts(b);
    term_set_cursor(TERM_CURSOR_BAR);
    term_puts("\n");
    term_cleanup();                                    /* the terminal's own cursor comes back */
    puts("done");
    return 0;
}
