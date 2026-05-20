/* Test structs, function pointers, string globals */
struct point {
    int x;
    int y;
};

int add_point(struct point *p) {
    return p->x + p->y;
}

int apply(int (*fn)(int, int), int a, int b) {
    return fn(a, b);
}

int mul(int a, int b) { return a * b; }
int sub(int a, int b) { return a - b; }

char *msg = "hello";

int main() {
    struct point p;
    int r;
    p.x = 30;
    p.y = 12;
    r = add_point(&p);          /* 42 */
    r = r + apply(mul, 3, 5);   /* 42 + 15 = 57 */
    r = r + apply(sub, 20, 8);  /* 57 + 12 = 69 */
    r = r + msg[0];             /* 69 + 'h'(104) = 173 */
    return r;
}
