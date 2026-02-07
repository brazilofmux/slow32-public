/* helper.c — defines globals exercising cross-file linker resolution */

struct point { int x; int y; };

int global_var = 42;

int helper_add(int a, int b) {
    return a + b;
}

int helper_mul(int a, int b) {
    return a * b;
}

const char *helper_string(void) {
    return "hello from helper";
}

int helper_call_back(int (*fn)(int)) {
    return fn(10);
}

void helper_fill_struct(struct point *p) {
    p->x = 100;
    p->y = 200;
}
