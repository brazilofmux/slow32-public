/* 7.27.3.5 strftime: %z and %Z of a localtime result, standard and daylight time, in America/Chicago */
#include <stdio.h>
#include <time.h>

int main(void) {
    static const long long when[] = {1736942400LL /* January */, 1752580800LL /* July */, 0};
    char buf[64];
    int i;
    for (i = 0; i < 3; i++) {
        time_t t = (time_t)when[i];
        strftime(buf, sizeof buf, "%z %Z", localtime(&t));
        printf("%lld: [%s]\n", when[i], buf);
    }
    return 0;
}
