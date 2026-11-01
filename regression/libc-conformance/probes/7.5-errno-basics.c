/* 7.5p2-p3: EDOM, EILSEQ, ERANGE are distinct positive #if constants; errno is 0 at startup and a modifiable lvalue */
#include <errno.h>
#include <stdio.h>

#if EDOM > 0 && EILSEQ > 0 && ERANGE > 0 && EDOM != ERANGE && EDOM != EILSEQ && ERANGE != EILSEQ
#define DISTINCT 1
#else
#define DISTINCT 0
#endif

int main(void) {
    int start = errno;
    int *p = &errno;
    errno = ERANGE;
    int a = (*p == ERANGE);
    *p = EDOM;
    int b = (errno == EDOM);
    errno += 0;
    printf("%d %d %d %d\n", DISTINCT, start == 0, a, b);
    return 0;
}
