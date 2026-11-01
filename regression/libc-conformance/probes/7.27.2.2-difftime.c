/* 7.27.2.2 difftime: time1 - time0 in seconds as a double, either sign */
#include <stdio.h>
#include <time.h>

int main(void) {
    double d = difftime((time_t)1000, (time_t)400);
    printf("%.1f\n", d);
    printf("%.1f\n", difftime((time_t)400, (time_t)1000));
    printf("%.1f\n", difftime((time_t)86400, (time_t)86400));
    printf("%.1f\n", difftime((time_t)0, (time_t)-86400));
    printf("%.1f\n", difftime((time_t)2147483647, (time_t)-2147483647 - 1));
    printf("is double: %d\n", sizeof difftime((time_t)0, (time_t)0) == sizeof(double));
    return 0;
}
