/* 7.22.4.6 getenv: a name the host environment sets is found, and asking twice gives the same string */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
    const char *a = getenv("TZ");
    char copy[64] = "";
    if (a) strncpy(copy, a, sizeof copy - 1);
    a = getenv("TZ");
    printf("TZ=%s same=%d\n", a ? a : "(null)", a && strcmp(a, copy) == 0);
    printf("prefix only: %d\n", getenv("T") == NULL);
    return 0;
}
