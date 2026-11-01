/* 7.11.1.1 setlocale: a request that cannot be honoured returns null and leaves the locale as it was */
#include <stdio.h>
#include <string.h>
#include <locale.h>

int main(void) {
    char before[256];
    char *p = setlocale(LC_ALL, NULL);
    strncpy(before, p ? p : "", 255); before[255] = 0;
    p = setlocale(LC_ALL, "no_such_locale.surely-not");
    printf("returned null %d ", p == NULL);
    p = setlocale(LC_ALL, NULL);
    printf("unchanged %d ", p != NULL && strcmp(p, before) == 0);
    printf("decimal point [%s]\n", localeconv()->decimal_point);
    return 0;
}
