/* 7.11.1.1 setlocale: a null locale queries without changing, "C" is always honoured, a returned name restores that locale */
#include <stdio.h>
#include <string.h>
#include <locale.h>

int main(void) {
    char start[256], c[256];
    char *p = setlocale(LC_ALL, NULL);
    printf("query %d ", p != NULL);
    if (!p) return 1;
    strncpy(start, p, 255); start[255] = 0;
    p = setlocale(LC_ALL, "C");
    printf("C %d ", p != NULL);
    if (!p) return 1;
    strncpy(c, p, 255); c[255] = 0;
    /* the program starts in the "C" locale, so the two names select the same locale */
    p = setlocale(LC_ALL, start);
    printf("restore %d ", p != NULL);
    p = setlocale(LC_NUMERIC, "C");
    printf("one category %d ", p != NULL);
    p = setlocale(LC_ALL, NULL);
    printf("query again %d\n", p != NULL && setlocale(LC_ALL, p) != NULL);
    return 0;
}
