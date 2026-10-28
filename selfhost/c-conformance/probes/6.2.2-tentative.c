// 6.2.2p5, 6.9.2p2  tentative definitions: repeated file-scope declarations without initializer are one object, zeroed
#include <stdio.h>
int t;
int t;
static int st;
static int st;
int main(void) {
    t++; st += 2;
    printf("%d %d %d\n", t, st, 0);
    return 0;
}
