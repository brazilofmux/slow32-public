/* 7.27.3.4p3 localtime: a time whose local year does not fit tm_year cannot be converted -- a null pointer */
#include <stdio.h>
#include <time.h>

static void one(long long v) {
    time_t t = (time_t)v;
    struct tm *g = localtime(&t);
    if (g) printf("%lld: year %lld\n", v, (long long)g->tm_year + 1900);
    else printf("%lld: null\n", v);
}

int main(void) {
    if (sizeof(time_t) != 8 || sizeof(int) != 4) {
        printf("not a 64-bit time_t with a 32-bit int: nothing to test\n");
        return 0;
    }
    one(9223372036854775807LL);
    one(-9223372036854775807LL - 1);
    one(67768036191676800LL + 86400);
    one(253402300800LL);       /* 10000-01-01 UTC: an ordinary time */
    return 0;
}
