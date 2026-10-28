// 6.10.3.3p4  a ## made by pasting # and # is an ordinary token, not the operator
#include <stdio.h>
#define hh # ## #
#define mk(a) # a
#define between(a) mk(a)
#define glue2(c, d) between(c hh d)
int main(void) {
    char s[] = glue2(p, q);
    puts(s);
    return 0;
}
