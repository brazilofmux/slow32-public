/* 7.22.5.1 bsearch: returns a matching element, a null pointer when none matches, and for an empty array */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static int cmp(const void *k, const void *e) {
    int x = *(const int *)k, y = *(const int *)e;
    return (x > y) - (x < y);
}
int main(void) {
    int a[] = {1, 3, 3, 3, 7, 9, 11, 20};
    int keys[] = {1, 3, 7, 20, 0, 2, 10, 21};
    int i, *p;
    for (i = 0; i < 8; i++) {
        p = bsearch(&keys[i], a, 8, sizeof a[0], cmp);
        printf("%d: %s\n", keys[i], p == NULL ? "none" : (*p == keys[i] && p >= a && p < a + 8) ? "found" : "WRONG");
    }
    p = bsearch(&keys[0], a, 0, sizeof a[0], cmp);
    printf("empty: %s\n", p ? "found" : "none");
    p = bsearch(&keys[2], a + 4, 1, sizeof a[0], cmp);
    printf("one: %s\n", p == a + 4 ? "found" : "none");
    p = bsearch(&keys[1], a + 4, 1, sizeof a[0], cmp);
    printf("one miss: %s\n", p ? "found" : "none");
    return 0;
}
