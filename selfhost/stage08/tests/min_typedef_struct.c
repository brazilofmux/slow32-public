struct pair {
    int a;
    int b;
};

typedef struct pair pair_t;

int sum(pair_t *p) {
    return p->a + p->b;
}

int main(void) {
    pair_t p;
    p.a = 1;
    p.b = 2;
    return sum(&p);
}
