// 6.10.3.2, 6.10.3.3  # and ## together: a field-name pasting printer, stringizing a literal with escapes, glue against a self-referential macro
#include <stdio.h>
#define S(v) #v
#define XS(v) S(v)
#define show(n, m) printf("v" #n "=%d v" #m "=%s\n", v ## n, v ## m)
#define join(p, q) p ## q
#define xjoin(p, q) join(p, q)
#define TOPEND "top"
#define END END " and more"
int main(void) {
    int v4 = 42;
    const char *v9 = "nine";
    show(4, 9);
    fputs(S(memcmp("x\0y", "x", '\3') /* gone */ != 1) S(; #\n), stdout);
    puts(join(TOP, END));                /* TOPEND, then expanded */
    puts(xjoin(TOP, END));               /* END expands once, then TOP pastes onto it */
    puts(XS(join(1, 2)));
    return 0;
}
