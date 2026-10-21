/* getenv keeps each name's answer in its own storage so a later call
 * does not overwrite an earlier one. The table grows past 64. */
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
    if (strcmp(first[0], "V00") != 0) {
        printf("clobber %s\n", first[0]);
        return 2;
    }
    if (getenv("S32_UNSET_XYZZY") != NULL) {
        printf("unset found\n");
        return 3;
    }
    printf("keep 80\n");
    return 0;
}
