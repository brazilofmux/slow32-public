/* Test #ifdef, #ifndef, #else, #endif */

#define HAVE_FEATURE 1

int main(void) {
    int x;

    /* Test #ifdef with defined symbol */
    x = 0;
#ifdef HAVE_FEATURE
    x = 1;
#endif
    if (x != 1) return 1;

    /* Test #ifdef with undefined symbol */
    x = 0;
#ifdef NOT_DEFINED
    x = 99;
#endif
    if (x != 0) return 2;

    /* Test #ifndef with undefined symbol */
    x = 0;
#ifndef NOT_DEFINED
    x = 1;
#endif
    if (x != 1) return 3;

    /* Test #ifndef with defined symbol */
    x = 0;
#ifndef HAVE_FEATURE
    x = 99;
#endif
    if (x != 0) return 4;

    /* Test #ifdef with #else */
    x = 0;
#ifdef HAVE_FEATURE
    x = 10;
#else
    x = 20;
#endif
    if (x != 10) return 5;

    /* Test #ifdef (false) with #else */
    x = 0;
#ifdef NOT_DEFINED
    x = 10;
#else
    x = 20;
#endif
    if (x != 20) return 6;

    /* Test nested #ifdef */
#define INNER 1
    x = 0;
#ifdef HAVE_FEATURE
#ifdef INNER
    x = 42;
#endif
#endif
    if (x != 42) return 7;

    /* Test nested: outer false */
    x = 0;
#ifdef NOT_DEFINED
#ifdef INNER
    x = 99;
#endif
#endif
    if (x != 0) return 8;

    return 0;
}
