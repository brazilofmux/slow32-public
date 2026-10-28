// 6.10.2p3  a "..." include not found locally falls back to the <...> search
#include "stddef.h"
#include "stdio.h"
int main(void) {
    printf("%d\n", (int)sizeof(size_t));
    return 0;
}
