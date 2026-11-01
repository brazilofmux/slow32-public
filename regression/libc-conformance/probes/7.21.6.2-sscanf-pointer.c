/* 7.21.6.2 fscanf: %p reads back what %p wrote and yields a pointer that compares equal to the original */
#include <stdio.h>

int main(void) {
    int obj;
    char buf[64];
    void *p = &obj, *q = NULL, *n = NULL;
    snprintf(buf, sizeof buf, "%p", p);
    int r = sscanf(buf, "%p", &q);
    printf("%d %d\n", r, q == p);
    snprintf(buf, sizeof buf, "%p", (void *)0);
    q = &obj;
    r = sscanf(buf, "%p", &q);
    printf("%d %d\n", r, q == n);
    return 0;
}
