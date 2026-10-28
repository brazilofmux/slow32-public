// 6.10.8.1p1  __FILE__ and __LINE__ inside an included header name that header
#include <stdio.h>
#include <string.h>
#include "inc/6.10.8-where.h"
static const char *base(const char *p) { const char *s = strrchr(p, '/'); return s ? s + 1 : p; }
int main(void) {
    printf("%s %d / %s %d\n", base(where_file()), where_line(), base(__FILE__), __LINE__);
    return 0;
}
