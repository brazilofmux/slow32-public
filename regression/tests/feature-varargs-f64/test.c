#include <stdio.h>
#include <stdarg.h>

static void dump(const char *tag, ...) {
    va_list ap;
    va_start(ap, tag);
    double d = va_arg(ap, double);
    union { double d; unsigned long long u; } v;
    v.d = d;
    printf("%s: %llx\n", tag, v.u);
    va_end(ap);
}

int main() {
    dump("a", 1.5);
    dump("b", 1.25);
    return 0;
}
