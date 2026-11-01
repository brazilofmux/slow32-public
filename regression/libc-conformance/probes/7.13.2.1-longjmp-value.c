/* 7.13.1.1p3, 7.13.2.1p4: setjmp returns 0 directly and longjmp's value after a jump; longjmp with 0 makes setjmp return 1 */
#include <setjmp.h>
#include <stdio.h>

static jmp_buf env;
static int i;

static void jump(int v) { longjmp(env, v); }

int main(void) {
    static const int vals[] = { 5, 0, -1, 2147483647 };
    switch (setjmp(env)) {
    case 0: printf("0\n"); break;
    case 1: printf("1\n"); break;
    case 5: printf("5\n"); break;
    case -1: printf("-1\n"); break;
    case 2147483647: printf("max\n"); break;
    default: printf("other\n"); break;
    }
    if (i < 4)
        jump(vals[i++]);
    return 0;
}
