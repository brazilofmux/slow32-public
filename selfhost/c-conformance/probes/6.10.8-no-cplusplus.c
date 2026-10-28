// 6.10.8p3  __cplusplus is not predefined
#include <stdio.h>
int main(void) {
#ifdef __cplusplus
    puts("defined");
#else
    puts("ok");
#endif
    return 0;
}
