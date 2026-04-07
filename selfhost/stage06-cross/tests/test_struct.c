/* Test struct member access, pointers to structs, arrays in structs */

struct point {
    int x;
    int y;
    int z;
};

int dot(struct point *a, struct point *b) {
    return a->x * b->x + a->y * b->y + a->z * b->z;
}

void init(struct point *p, int x, int y, int z) {
    p->x = x;
    p->y = y;
    p->z = z;
}

int main(int argc, char **argv) {
    struct point a;
    struct point b;
    init(&a, 1, 2, 3);
    init(&b, 4, 5, 6);
    return dot(&a, &b);  /* 4 + 10 + 18 = 32 */
}
