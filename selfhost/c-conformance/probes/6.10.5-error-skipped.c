// 6.10.5p1,6.10.1p6  #error in a skipped group has no effect
#include <stdio.h>
#if 0
#error never seen
#elif defined(NOT_DEFINED_ANYWHERE)
#error nor this
#endif
int main(void) { puts("ok"); return 0; }
