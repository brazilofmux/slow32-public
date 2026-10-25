/* A frame slot that is only ever stored to loses its stores; one that
 * is read, passed to a callee, has its address kept, or lives in a
 * function that calls setjmp keeps them.  run-tests.sh checks that
 * scratch stores nothing. */
#include <setjmp.h>

int *kept;
jmp_buf env;

int scratch(int a) {
    int w[6];
    long long big;
    w[0] = a;
    w[3] = a * 2;
    w[5] = a + 9;
    big = (long long)a * 1000000007LL;
    return a * 3;
}

static int sum3(int *p) { return p[0] + p[1] + p[2]; }

int passed(int a) {
    int w[3];
    w[0] = a;
    w[1] = a + 1;
    w[2] = a + 2;
    return sum3(w);
}

int escaped(int a) {
    static int box[2];
    int w[2];
    w[0] = a;
    w[1] = a * a;
    kept = box;
    box[0] = w[0];
    box[1] = w[1];
    return 0;
}

int leaked(int a) {
    int w[2];
    int *q;
    w[0] = a + 4;
    w[1] = a + 5;
    q = w;
    kept = q;
    return 0;
}

int partly(int a, int k) {
    int w[4];
    w[0] = a;
    w[1] = a + 1;
    w[2] = a + 2;
    w[3] = a + 3;
    return w[k & 3];
}

int jumped(int a) {
    int w[2];
    w[0] = a;
    if (setjmp(env) == 0) {
        w[0] = a + 50;
        longjmp(env, 1);
    }
    return w[0];
}

int main(void) {
    if (scratch(7) != 21) return 1;
    if (passed(10) != 33) return 2;
    escaped(6);
    if (kept[0] != 6 || kept[1] != 36) return 3;
    if (partly(20, 2) != 22 || partly(20, 7) != 23) return 4;
    if (jumped(5) != 55) return 5;
    return 0;
}
