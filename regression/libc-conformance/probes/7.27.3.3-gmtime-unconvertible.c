/* 7.27.3.3p3 gmtime: a time whose year does not fit tm_year cannot be converted -- a null pointer; the last that fits can */
#include <stdio.h>
#include <time.h>

static void one(long long v) {
    time_t t = (time_t)v;
    struct tm *g = gmtime(&t);
    if (g) printf("%lld: year %lld %02d-%02d\n", v, (long long)g->tm_year + 1900, g->tm_mon + 1, g->tm_mday);
    else printf("%lld: null\n", v);
}

int main(void) {
    if (sizeof(time_t) != 8 || sizeof(int) != 4) {
        printf("not a 64-bit time_t with a 32-bit int: nothing to test\n");
        return 0;
    }
    one(67768036191676799LL);    /* 2147485547-12-31 23:59:59: tm_year INT_MAX */
    one(67768036191676800LL);    /* one second later */
    one(9223372036854775807LL);
    one(-67768040609740800LL);   /* -2147481748-01-01: tm_year INT_MIN */
    one(-67768040609740801LL);
    one(-9223372036854775807LL - 1);
    return 0;
}
