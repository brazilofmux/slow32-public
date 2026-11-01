/* 7.22.5.1 bsearch: the key need not have the element type; the comparison gets the key first */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

struct ent { const char *name; int val; };
static const struct ent tab[] = { {"apple", 1}, {"fig", 2}, {"kiwi", 3}, {"lime", 4}, {"pear", 5} };
static int cmp(const void *k, const void *e) { return strcmp((const char *)k, ((const struct ent *)e)->name); }
int main(void) {
    const char *q[] = {"apple", "kiwi", "pear", "plum", "aaa", "grape"};
    int i;
    for (i = 0; i < 6; i++) {
        const struct ent *p = bsearch(q[i], tab, 5, sizeof tab[0], cmp);
        printf("%s -> %d\n", q[i], p ? p->val : -1);
    }
    return 0;
}
