// 6.4.2.2p1  __func__ is a static const char array holding the function name
#include <stdio.h>
#include <string.h>
static const char *who(void) { return __func__; }
int main(void) {
    printf("%s %s %d %d\n", __func__, who(), (int)sizeof __func__, (int)strlen(who()));
    return 0;
}
