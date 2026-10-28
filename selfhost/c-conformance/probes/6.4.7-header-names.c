// 6.4.7p1-p3  both header-name forms; "..." searches the source's directory first; macro-expanded #include
#include <stdio.h>
#include "stddef.h"
#define HDR <string.h>
#include HDR
int main(void) {
    printf("%d %d\n", (int)strlen("four"), (int)sizeof(size_t));
    return 0;
}
