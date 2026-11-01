/* 7.1.2 standard headers: <float.h> and <iso646.h> included twice each, in either order */
#include <iso646.h>
#include <float.h>
#include <iso646.h>
#include <float.h>
#include <stdio.h>

int main(void) {
    printf("%d %d\n", FLT_RADIX >= 2 and DBL_DIG >= 10, 1 bitand 3);
    return 0;
}
