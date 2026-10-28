// 6.10.2p4  #include of macro-replaced pp-tokens ("..." form)
#include <stdio.h>
#define VERSION 2
#if VERSION == 1
#  define INCFILE "inc/6.10.2-v1.h"
#else
#  define INCFILE "inc/6.10.2-v2.h"
#endif
#include INCFILE
int main(void) {
    printf("%d\n", CHOSEN);
    return 0;
}
