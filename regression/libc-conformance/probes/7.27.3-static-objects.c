/* 7.27.3p1 conversion functions: gmtime and localtime return pointers to library-owned static objects, not null for ordinary times */
#include <stdio.h>
#include <time.h>

int main(void) {
    time_t a = 1000000000, b = 2000000000;
    struct tm *p = gmtime(&a);
    int ya = p->tm_year;
    struct tm *q = gmtime(&b);
    printf("first year %d, second year %d\n", ya + 1900, q->tm_year + 1900);
    printf("non-null: %d %d\n", p != NULL, localtime(&a) != NULL);
    printf("asctime text ends in newline: %d\n", asctime(gmtime(&a))[24] == '\n');
    return 0;
}
