// 6.4.1p1  the C89/C99 keywords are recognized (each used once); C11 _-keywords probed separately
#include <stdio.h>
static inline int sq(int x) { return x * x; }
int main(void) {
    auto int a = 2;
    register int reg = 1;
    const volatile int cv = 3;
    int *restrict rp = &a;
    _Bool bl = 1;
    _Static_assert(sizeof(int) == 4, "int");
    signed short ss = -1; unsigned long ul = 2; long long ll = 3; float f = 1.0f; double d = 2.0;
    char ch = 'c';
    typedef union { int i; } U; U u = { 1 };
    enum { K = 4 };
    struct { int m; } st = { 5 };
    extern int printf(const char *, ...);
    static int sv = 6;
    int r = 0;
    do { r++; if (r > 3) break; else continue; } while (r < 10);
    switch (r) { case 4: r += 1; break; default: goto out; }
    for (; reg < 2; reg++) r++;
out:
    printf("%d %d %d %d %d %lu %lld %.0f %.0f %c %d %d %d %d %d\n", sq(a), cv, *rp, bl, ss, ul, ll, f, d, ch,
           u.i, K, st.m, r, sv);
    return 0;
}
