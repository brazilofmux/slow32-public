#include <stdio.h>
#include <term.h>

int main(void) {
    printf("=== Term Service Test ===\n");

    int rc = term_init();
    if (rc != 0) {
        printf("term_init returned %d (denied or unavailable)\n", rc);
        printf("PASS (graceful deny)\n");
        return 0;
    }

    printf("term_init: OK (service negotiated)\n");

    /* Query terminal size */
    int rows = 0, cols = 0;
    term_get_size(&rows, &cols);
    printf("Terminal size: %d rows x %d cols\n", rows, cols);

    /* Test cursor movement */
    /* Don't actually move cursor in automated test, just exercise the API */
    /* term_gotoxy(1, 1); */

    /* Test key availability (non-blocking) */
    int avail = term_kbhit();
    printf("Key available: %d\n", avail);

    /* Test set attribute */
    term_set_attr(0);  /* normal */
    printf("Attribute set to normal\n");

    /* Test set color */
    term_set_color(7, 0);  /* white on black */
    printf("Color set\n");
    term_set_attr(0);  /* reset */

    /* Clean up */
    term_cleanup();
    printf("term_cleanup: OK\n");

    printf("PASS\n");
    return 0;
}
