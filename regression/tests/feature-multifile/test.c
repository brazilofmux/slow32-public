/* test.c — exercises cross-file linking with extern symbols */
#include <stdio.h>
#include <string.h>

struct point { int x; int y; };

/* Extern declarations for helper.c symbols */
extern int global_var;
extern int helper_add(int a, int b);
extern int helper_mul(int a, int b);
extern const char *helper_string(void);
extern int helper_call_back(int (*fn)(int));
extern void helper_fill_struct(struct point *p);

static int double_it(int v) {
    return v * 2;
}

int main(void) {
    /* Cross-file function calls */
    printf("%s: cross-file add\n", helper_add(3, 4) == 7 ? "PASS" : "FAIL");
    printf("%s: cross-file mul\n", helper_mul(5, 6) == 30 ? "PASS" : "FAIL");

    /* Cross-file global variable read */
    printf("%s: global read\n", global_var == 42 ? "PASS" : "FAIL");

    /* Cross-file global variable write */
    global_var = 99;
    printf("%s: global write\n", global_var == 99 ? "PASS" : "FAIL");

    /* Cross-file string literal access */
    const char *s = helper_string();
    printf("%s: cross-file string\n", strcmp(s, "hello from helper") == 0 ? "PASS" : "FAIL");

    /* Function pointer callback across files */
    printf("%s: callback\n", helper_call_back(double_it) == 20 ? "PASS" : "FAIL");

    /* Cross-file struct pointer passing */
    struct point p;
    helper_fill_struct(&p);
    printf("%s: struct fill\n", (p.x == 100 && p.y == 200) ? "PASS" : "FAIL");

    return 0;
}
