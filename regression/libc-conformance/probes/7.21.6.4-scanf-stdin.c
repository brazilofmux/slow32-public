/* 7.21.6.4 scanf: fscanf with stdin; vscanf and vfscanf are its va_list forms */
#include <stdio.h>
#include <stdarg.h>

static int vs(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int r = vscanf(fmt, ap);
    va_end(ap);
    return r;
}

static int vf(FILE *f, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int r = vfscanf(f, fmt, ap);
    va_end(ap);
    return r;
}

int main(void) {
    int a = 0;
    char w[8] = "";
    double d = 0;
    int r = scanf("%d", &a);
    int r2 = vs("%7s", w);
    int r3 = vf(stdin, "%lf", &d);
    printf("%d %d %d: %d %s %g\n", r, r2, r3, a, w, d);
    return 0;
}
