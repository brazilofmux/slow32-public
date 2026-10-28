// 6.8.3p2-4  expression statements evaluated as void for side effects; null statements as loop bodies and before }
#include <stdio.h>
static int calls;
static int f(void) { return ++calls; }
int main(void) {
    int i, x = 0;
    for (i = 0; i < 10; i++)
        ;
    f();
    (void)f();
    x++, f(), x += 2;
    char s[] = "hello";
    char *p = s;
    while (*p++)
        ;
    ;;;
    {
        goto out;
    out: ;
    }
    printf("%d %d %d %d\n", i, x, calls, (int)(p - s));
    return 0;
}
