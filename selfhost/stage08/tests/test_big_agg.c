/* Large aggregates: `= {0}` on a 16KB struct, struct assignment, return
 * by value and a byval parameter must be memset/memcpy calls, not one
 * instruction per word (regal's ReconcileResult made reconcile() 178k
 * instructions).  Small structs keep the inline word copies. */
#include <string.h>
typedef struct { int n; long long total; int a[2048]; int b[2048]; } Big;
typedef struct { short x; char y; } Small;
static Big g;
static Big make(int seed) {
    Big r = {0};
    int i;
    r.n = seed;
    r.total = seed * 1000LL;
    for (i = 0; i < 2048; i++) { r.a[i] = i * seed; r.b[i] = -i; }
    return r;
}
static int sum(Big v) { int i, s = v.n; for (i = 0; i < 2048; i += 256) s += v.a[i] + v.b[i]; return s; }
static Small mk(char c) { Small s = {0}; s.x = 7; s.y = c; return s; }
int main(void) {
    Big z = {0};
    Small sm;
    int i;
    for (i = 0; i < 2048; i += 100) if (z.a[i] != 0 || z.b[i] != 0) return 1;
    if (z.n != 0 || z.total != 0) return 2;
    g = make(3);
    if (g.n != 3 || g.total != 3000 || g.a[2047] != 6141 || g.b[5] != -5) return 3;
    z = g;
    if (z.a[1000] != 3000 || z.b[2047] != -2047) return 4;
    if (sum(z) != 3 + (0+256+512+768+1024+1280+1536+1792) * 3 - (0+256+512+768+1024+1280+1536+1792)) return 5;
    sm = mk('q');
    if (sm.x != 7 || sm.y != 'q') return 6;
    return 0;
}
