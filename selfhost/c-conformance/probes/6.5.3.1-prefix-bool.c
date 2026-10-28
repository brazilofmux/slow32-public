// 6.5.3.1p2 (6.3.1.2)  prefix ++ on a _Bool that is already 1 leaves 1
#include <stdio.h>
int main(void) { _Bool b = 1; ++b; _Bool c = 1; --c; --c; printf("%d %d\n", b, c); return 0; }
