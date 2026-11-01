/* 7.27.3.2 ctime: the same text as asctime(localtime(timer)), local time of the zone */
#include <stdio.h>
#include <string.h>
#include <time.h>

int main(void) {
    static const long long when[] = {0, 1736942400LL /* 2025-01-15 12:00 UTC */, 1752580800LL /* 2025-07-15 */,
                                     -1000000000LL, 2524651200LL /* 2050 */, 4102444800LL /* 2100 */};
    char a[64], b[64];
    int i;
    for (i = 0; i < (int)(sizeof when / sizeof when[0]); i++) {
        time_t t = (time_t)when[i];
        strcpy(a, ctime(&t));
        strcpy(b, asctime(localtime(&t)));
        printf("%lld: same=%d %s", when[i], strcmp(a, b) == 0, a);
    }
    return 0;
}
