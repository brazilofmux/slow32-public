// 6.10.3.3p3  ## also works in an object-like macro's list
#include <stdio.h>
#define JOINED ab ## cd
int main(void) {
    int abcd = 77;
    printf("%d\n", JOINED);
    return 0;
}
