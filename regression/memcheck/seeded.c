/* seeded.c -- one memory error for each argument, and none for 0: what
 * run-memcheck.sh holds the interpreter's -m to.  Built without
 * optimization, so that each error is still there to be seen. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct pair { char tag; int n; };

/* the index is the caller's, so no compiler keeps x in registers */
static int stale(int i) { int x[4]; x[i & 1] = 3; return x[2 | (i & 1)]; }
static int *dangling(void) { int local[8]; local[0] = 42; return local + (local[0] - 42); }
static int deep(int n) { volatile int pad[32]; pad[0] = n; return n ? deep(n - 1) + pad[0] : 0; }

int main(int argc, char **argv)
{
    int which = argc > 1 ? atoi(argv[1]) : 0;
    char *p = malloc(10);
    switch (which) {
    case 0: {       /* nothing wrong: a structure with padding copied, a block
                       grown and shrunk, a select the backends do without a branch */
        struct pair a, b; char *q; int i, n = 0;
        a.tag = 'x'; a.n = 5; b = a;
        strcpy(p, "ok");
        q = malloc(1);
        for (i = 1; i < 200; i++) { q = realloc(q, (size_t)i); memset(q, i, (size_t)i); n += q[i - 1]; }
        q = realloc(q, 7); n += q[6];
        printf("clean %s %c %d %d %d\n", p, b.tag, b.n, n, deep(20));
        free(q);
        break;
    }
    case 1: if (stale(which) > 0) puts("branch on a stale local"); else puts("branch on a stale local."); break;
    case 2: p[10] = 1; puts("one past a block"); break;
    case 3: free(p); printf("after free %d\n", p[1] != 0); return 0;
    case 4: free(p); free(p); puts("freed twice"); return 0;
    case 5: { struct pair *q = 0; printf("null %d\n", q->n != 0); break; }
    case 6: p[0] = 'a'; p[4] = '\n'; fwrite(p, 1, 5, stdout); break;
    case 7: { int *q = malloc(40); puts(q[3] == 7 ? "heap word" : "heap word."); free(q); break; }
    case 8: { char *q = realloc(p, 100); q[50] = 1; puts(q[60] ? "grown tail" : "grown tail."); p = q; break; }
    case 9: { int *q = dangling(); deep(4); printf("dead frame %d\n", *q != 0); break; }
    case 10: { int local; free(&local); puts("free of a local"); break; }
    case 11: { char *q = p - 4; printf("before a block %d\n", *q != 0); break; }
    }
    free(p);
    return 0;
}
