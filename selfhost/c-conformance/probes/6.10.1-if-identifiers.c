// 6.10.1p4  remaining identifiers (even keyword spellings) become 0 in #if
#include <stdio.h>
#define SEVEN 7
#define EXPR (SEVEN * 2)
int main(void) {
    int r = 0;
#if not_a_macro == 0
    r |= 1;
#endif
#if int + while == 0
    r |= 2;
#endif
#if EXPR == 14
    r |= 4;
#endif
#if true
    r |= 8;                     /* true is not a macro here: must be skipped */
#endif
    printf("%d\n", r);
    return 0;
}
