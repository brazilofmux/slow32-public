// 6.8.4.2p7  a switch may jump past declarations (not VLAs) in its body; the objects exist uninitialized
#include <stdio.h>
int main(void) {
    int out = 0;
    for (int k = 0; k < 3; k++) {
        switch (k) {
            int t = 42;
        case 0:
            t = 5;
            out += t;
            break;
        case 1: {
            int u = k * 7;
            out += u;
        }
        /* fall through */
        default:
            t = 100;
            out += t;
        }
    }
    printf("%d\n", out);
    return 0;
}
