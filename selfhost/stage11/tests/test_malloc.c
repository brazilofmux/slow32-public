/* Test malloc/free/calloc/realloc */

char *malloc(int size);
void free(char *ptr);
char *calloc(int nmemb, int size);
char *realloc(char *ptr, int size);
char *memset(char *dst, int c, int n);

/* Simple linked list node */
struct node {
    int value;
    struct node *next;
};

/* Test basic malloc and free */
int test_basic_alloc(void) {
    int *p;
    p = (int *)malloc(4);
    if (!p) return 1;
    *p = 42;
    if (*p != 42) return 2;
    free((char *)p);
    return 0;
}

/* Test multiple allocations */
int test_multi_alloc(void) {
    int *a;
    int *b;
    int *c;
    a = (int *)malloc(4);
    b = (int *)malloc(4);
    c = (int *)malloc(4);
    if (!a || !b || !c) return 1;
    *a = 10;
    *b = 20;
    *c = 30;
    if (*a != 10) return 2;
    if (*b != 20) return 3;
    if (*c != 30) return 4;
    /* Verify they don't overlap */
    if (a == b || b == c || a == c) return 5;
    free((char *)a);
    free((char *)b);
    free((char *)c);
    return 0;
}

/* Test calloc zeros memory */
int test_calloc(void) {
    char *p;
    int i;
    p = calloc(32, 1);
    if (!p) return 1;
    i = 0;
    while (i < 32) {
        if (p[i] != 0) return 2;
        i = i + 1;
    }
    free(p);
    return 0;
}

/* Test linked list with malloc */
int test_linked_list(void) {
    struct node *head;
    struct node *n;
    struct node *tmp;
    int i;
    int sum;
    /* Build list: 0 -> 1 -> 2 -> 3 -> 4 */
    head = (struct node *)0;
    i = 4;
    while (i >= 0) {
        n = (struct node *)malloc(8);
        if (!n) return 1;
        n->value = i;
        n->next = head;
        head = n;
        i = i - 1;
    }
    /* Walk list and sum values */
    sum = 0;
    n = head;
    while (n) {
        sum = sum + n->value;
        n = n->next;
    }
    if (sum != 10) return 2;
    /* Free all nodes */
    n = head;
    while (n) {
        tmp = n->next;
        free((char *)n);
        n = tmp;
    }
    return 0;
}

/* Test realloc grows allocation */
int test_realloc(void) {
    char *p;
    p = malloc(8);
    if (!p) return 1;
    p[0] = 65;
    p[1] = 66;
    p[2] = 67;
    p = realloc(p, 64);
    if (!p) return 2;
    /* Original data should be preserved */
    if (p[0] != 65) return 3;
    if (p[1] != 66) return 4;
    if (p[2] != 67) return 5;
    free(p);
    return 0;
}

/* Test alloc-free-alloc reuse */
int test_free_reuse(void) {
    char *p1;
    char *p2;
    p1 = malloc(16);
    if (!p1) return 1;
    free(p1);
    p2 = malloc(16);
    if (!p2) return 2;
    /* Should reuse freed memory (p2 could equal p1) */
    *((int *)p2) = 99;
    if (*((int *)p2) != 99) return 3;
    free(p2);
    return 0;
}

/* Test many small allocations (stress) */
int test_many_allocs(void) {
    int i;
    char *ptrs[64];
    i = 0;
    while (i < 64) {
        ptrs[i] = malloc(32);
        if (!ptrs[i]) return 1;
        memset(ptrs[i], i, 32);
        i = i + 1;
    }
    /* Verify each block */
    i = 0;
    while (i < 64) {
        if (ptrs[i][0] != (char)i) return 2;
        if (ptrs[i][31] != (char)i) return 3;
        i = i + 1;
    }
    /* Free all */
    i = 0;
    while (i < 64) {
        free(ptrs[i]);
        i = i + 1;
    }
    return 0;
}

int main(void) {
    int rc;
    rc = test_basic_alloc();
    if (rc) return rc;
    rc = test_multi_alloc();
    if (rc) return rc + 10;
    rc = test_calloc();
    if (rc) return rc + 20;
    rc = test_linked_list();
    if (rc) return rc + 30;
    rc = test_realloc();
    if (rc) return rc + 40;
    rc = test_free_reuse();
    if (rc) return rc + 50;
    rc = test_many_allocs();
    if (rc) return rc + 60;
    return 0;
}
