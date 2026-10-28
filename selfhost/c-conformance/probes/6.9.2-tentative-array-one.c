// 6.9.2p2,p5  a tentative definition of an incomplete array becomes a one-element zeroed array
#include <stdio.h>
int i[];
int j = 7;
int main(void) {
    i[0] += 3;
    printf("%d %d\n", i[0], j);
    return 0;
}
