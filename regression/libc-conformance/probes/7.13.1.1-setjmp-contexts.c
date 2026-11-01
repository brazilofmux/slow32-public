/* 7.13.1.1p4: setjmp works in each permitted context: whole controlling expression, compared with a constant, operand of !, expression statement */
#include <setjmp.h>
#include <stdio.h>

static jmp_buf env;
static int hops;

static void back(int v) { hops++; longjmp(env, v); }

int main(void) {
    volatile int n = 0;
    if (setjmp(env)) {
        printf("if-taken ");
    } else {
        back(4);
    }
    switch (setjmp(env)) {
    case 0: back(2); break;
    case 2: printf("switch-2 "); break;
    default: printf("switch-bad "); break;
    }
    if (setjmp(env) == 3)
        printf("eq-3 ");
    else
        back(3);
    if (!setjmp(env))
        back(9);
    else
        printf("not ");
    n = 0;
    setjmp(env);
    if (++n < 4)
        back(1);
    printf("stmt-%d ", (int)n);
    while (setjmp(env) < 5) {
        static int w;
        if (++w > 3) break;
        back(w);
    }
    printf("while\nhops %d\n", hops);
    return 0;
}
