/* 7.22.4.2 atexit: at least 32 registrations succeed, and all of them run, last first */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static int n;
#define F(i) static void f##i(void) { printf("%d%c", i, i == 0 ? '\n' : ' '); }
F(0) F(1) F(2) F(3) F(4) F(5) F(6) F(7) F(8) F(9) F(10) F(11) F(12) F(13) F(14) F(15)
F(16) F(17) F(18) F(19) F(20) F(21) F(22) F(23) F(24) F(25) F(26) F(27) F(28) F(29) F(30) F(31)
static void (*fs[32])(void) = { f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15,
    f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, f27, f28, f29, f30, f31 };
int main(void) {
    int i;
    for (i = 0; i < 32; i++) if (atexit(fs[i]) == 0) n++;
    printf("registered %d\n", n);
    return 0;
}
