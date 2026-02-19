/* test9.c — include headers and edge cases */
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

int putchar(int c);

void print_int(int n) {
    if (n < 0) { putchar(45); n = 0 - n; }
    if (n >= 10) print_int(n / 10);
    putchar(48 + n % 10);
}

void print_nl() { putchar(10); }
void print_sp() { putchar(32); }

/* Test typedef from stdint.h */
uint32_t add_u32(uint32_t a, uint32_t b) {
    return a + b;
}

/* Test bool from stdbool.h */
bool is_positive(int x) {
    return x > 0;
}

/* Test NULL */
int test_null() {
    int *p = NULL;
    if (p == NULL) return 1;
    return 0;
}

/* Test multiple declarators */
int test_multi_decl() {
    int a, b, c;
    a = 10;
    b = 20;
    c = 30;
    return a + b + c;
}

/* Test sizeof on types and expressions */
int test_sizeof() {
    int x;
    return sizeof(int) * 100 + sizeof(char) * 10 + sizeof(x);
}

/* Test array of structs */
struct pair {
    int key;
    int val;
};

int test_array_of_structs() {
    struct pair arr[3];
    arr[0].key = 10;
    arr[0].val = 1;
    arr[1].key = 20;
    arr[1].val = 2;
    arr[2].key = 30;
    arr[2].val = 3;
    int sum = 0;
    int i;
    for (i = 0; i < 3; i++) {
        sum += arr[i].val;
    }
    return sum;
}

/* Test address-of struct member */
int test_addr_of_member() {
    struct pair p;
    p.key = 42;
    p.val = 99;
    int *pk = &p.key;
    int *pv = &p.val;
    return *pk + *pv;
}

/* Test nested ternary */
int classify(int x) {
    return x > 0 ? 1 : (x < 0 ? -1 : 0);
}

/* Test comma operator */
int test_comma() {
    int x;
    x = (1, 2, 3);
    return x;
}

int main() {
    print_int(add_u32(100, 200));  /* 300 */
    print_sp();
    print_int(is_positive(5));     /* 1 */
    print_sp();
    print_int(is_positive(-3));    /* 0 */
    print_sp();
    print_int(test_null());        /* 1 */
    print_sp();
    print_int(test_multi_decl());  /* 60 */
    print_sp();
    print_int(test_sizeof());      /* 414 */
    print_sp();
    print_int(test_array_of_structs());  /* 6 */
    print_sp();
    print_int(test_addr_of_member());    /* 141 */
    print_sp();
    print_int(classify(5));        /* 1 */
    print_sp();
    print_int(classify(-3));       /* -1 */
    print_sp();
    print_int(classify(0));        /* 0 */
    print_sp();
    print_int(test_comma());       /* 3 */
    print_nl();
    return 0;
}
