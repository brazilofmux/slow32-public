/* getenv keeps each name's answer in its own storage so a later call
 * does not overwrite an earlier one, past the 64 names the table once
 * stopped at. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
    char name[8], want[8];
    char *first[80];
    int i;

    for (i = 0; i < 80; i++) {
        sprintf(name, "K%02d", i);
        sprintf(want, "V%02d", i);
        first[i] = getenv(name);
        if (!first[i] || strcmp(first[i], want) != 0) {
            printf("miss %s\n", name);
            return 1;
        }
    }
    /* every answer still its own, the ones past 64 above all: those
     * once shared one buffer, so each held the last value asked for */
    for (i = 0; i < 80; i++) {
        sprintf(want, "V%02d", i);
        if (strcmp(first[i], want) != 0) {
            printf("clobber K%02d: %s\n", i, first[i]);
            return 2;
        }
    }
    if (getenv("S32_UNSET_XYZZY") != NULL) {
        printf("unset found\n");
        return 3;
    }
    printf("keep 80\n");
    return 0;
}
