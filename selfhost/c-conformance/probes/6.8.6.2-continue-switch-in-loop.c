// 6.8.6.2p2  continue inside a switch inside a loop continues the loop
#include <stdio.h>
int main(void) {
    int s = 0;
    for (int i = 0; i < 6; i++) {
        switch (i % 3) {
        case 0: continue;
        case 1: s += 1; break;
        default: s += 10;
        }
        s += 100;
    }
    printf("%d\n", s);
    return 0;
}
