// 6.10.1p1,p5  defined X, defined(X), #ifdef, #ifndef; defined operand not expanded
#include <stdio.h>
#define HAVE 0
#define ALIAS NOPE
int main(void) {
    int r = 0;
#if defined HAVE && defined(HAVE) && !defined NOPE
    r |= 1;
#endif
#if defined ALIAS            /* ALIAS itself is defined; not replaced first */
    r |= 2;
#endif
#ifdef HAVE
    r |= 4;
#endif
#ifndef NOPE
    r |= 8;
#endif
#undef HAVE
#ifdef HAVE
    r |= 16;
#endif
#if defined ( ALIAS ) + defined NOPE == 1
    r |= 32;
#endif
    printf("%d\n", r);
    return 0;
}
