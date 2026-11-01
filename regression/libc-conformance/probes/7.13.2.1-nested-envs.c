/* 7.13.2.1p2: longjmp to a jmp_buf of an inner active function, and to an outer one past the inner */
#include <setjmp.h>
#include <stdio.h>

static jmp_buf outer;

static void inner_work(jmp_buf inner, int k) {
    if (k == 1) longjmp(inner, 11);
    if (k == 2) longjmp(outer, 22);
}

static int middle(int k) {
    jmp_buf inner;
    switch (setjmp(inner)) {
    case 0: inner_work(inner, k); return -1;
    case 11: return 111;
    default: return -2;
    }
}

int main(void) {
    printf("%d\n", middle(1));
    switch (setjmp(outer)) {
    case 0:
        printf("%d\n", middle(2));
        printf("not reached\n");
        break;
    case 22:
        printf("outer 22\n");
        break;
    default:
        printf("outer other\n");
        break;
    }
    printf("%d\n", middle(0));
    return 0;
}
