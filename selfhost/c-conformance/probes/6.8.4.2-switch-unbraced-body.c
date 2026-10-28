// 6.8.4.2p4  the switch body may be any statement, e.g. a single labeled statement without braces
#include <stdio.h>
static int g(int x) { int r = 7; switch (x) default: r += 3; return r; }
static int h(int x) { int r = 7; switch (x) case 9: r = 99; return r; }
int main(void) {
    printf("%d %d %d\n", g(1), h(1), h(9));
    return 0;
}
