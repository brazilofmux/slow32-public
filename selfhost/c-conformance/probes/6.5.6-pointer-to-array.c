// 6.5.6p8/6.5.3.2p4  a pointer to an array steps by the whole array, and *p is the array
#include <stdio.h>
int a[3][3] = { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } };
struct s { int (*rows)[3]; };
static int f(int (*p)[3]) {
    int (*q)[3] = p + 1;
    int n = 0;
    n = n * 10 + (*p)[2];
    n = n * 10 + (*(p + 2))[0];
    n = n * 10 + q[0][1];
    q++;
    n = n * 10 + (*q)[1];
    q -= 2;
    n = n * 10 + q[2][2];
    n = n * 10 + (int)(q + 2 - p);
    return n;
}
int main(void) {
    struct s w;
    w.rows = a;
    printf("%d %u %u\n", f(a), (unsigned)sizeof *w.rows, (unsigned)sizeof(*w.rows)[0]);
    printf("%d %d\n", (*(w.rows + 1))[2], w.rows[2][0]);
    printf("%d %d %d %d %u %u\n", (*a)[1], (*(a + 1))[1], **a, *(*(a + 2) + 1),
           (unsigned)sizeof *a, (unsigned)sizeof(a + 1));
    {
        int l[2][4] = { { 1, 2, 3, 4 }, { 5, 6, 7, 8 } };
        static short t[3][2] = { { 9, 8 }, { 7, 6 }, { 5, 4 } };
        printf("%d %d %u %d %d %u\n", (*(l + 1))[3], **(l + 1), (unsigned)sizeof *l,
               (*(t + 2))[1], 1[t][0], (unsigned)sizeof *t);
    }
    return 0;
}
