// 6.9.2p2  a tentative definition of an incomplete array, completed by a later declaration
#include <stdio.h>
int sized[];
int sized[4];
int main(void) {
    printf("%d %d\n", (int)sizeof sized, sized[3]);
    return 0;
}
