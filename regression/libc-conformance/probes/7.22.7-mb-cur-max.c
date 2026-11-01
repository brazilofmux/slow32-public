/* 7.22p3 MB_CUR_MAX: positive, no more than MB_LEN_MAX, and 1 in the C locale here */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

int main(void) {
    printf("%d %d\n", MB_CUR_MAX >= 1, MB_CUR_MAX <= MB_LEN_MAX);
    return 0;
}
