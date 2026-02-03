#include <stdio.h>

int main() {
    union {
        double d;
        unsigned int w[2];
    } u;
    u.d = 1.25;
    printf("w0=%08x w1=%08x\n", u.w[0], u.w[1]);
    return 0;
}
