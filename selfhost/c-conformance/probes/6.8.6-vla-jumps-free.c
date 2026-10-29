// 6.8.6.1p1/6.2.4p7  leaving a VLA's scope by break, continue or goto frees it: a loop that allocated each time without freeing would exhaust the stack
#include <stdio.h>
static int total;
static char *lo, *hi;
/* where each VLA lands: freed, the same place every time round */
static void use(char *p, int n) {
    p[0] = 1; p[n - 1] = 2; total += p[0] + p[n - 1];
    if (!lo || p < lo) lo = p;
    if (!hi || p > hi) hi = p;
}
int main(void) {
    int i;
    for (i = 0; i < 20000; i++) {
        char a[4000 + (i & 0)];
        use(a, (int)sizeof a);
        if (i & 1) continue;
        {
            char b[3000];
            use(b, 3000);
            if (i % 3 == 0) continue;
        }
    }
    for (i = 0; ; i++) {
        char c[5000 + i % 1];
        use(c, (int)sizeof c);
        if (i == 19999) break;
    }
    i = 0;
again:
    {
        char d[6000 + (i & 0)];
        use(d, (int)sizeof d);
        i++;
        if (i < 20000) goto again;
    }
    do {
        char e[2500];
        use(e, 2500);
        i--;
        switch (i & 3) {
        case 0: { char f[4000]; use(f, 4000); break; }
        default: break;
        }
    } while (i > 0);
    printf("%d %d\n", total, (int)(hi - lo) < 30000);
    return 0;
}
