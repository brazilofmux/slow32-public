/* 7.22.1.1 atof: skips white space, converts the longest prefix like strtod, returns 0 when nothing converts */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(void) {
    printf("%d %d %d %d\n", atof("  1.5xyz") == 1.5, atof("-0x1p3") == -8.0,
           atof("abc") == 0.0, atof("\t+2.5e1") == 25.0);
    return 0;
}
