struct pair {
    int a, b;
    char c, d;
};

int main(void) {
    struct pair p;
    p.a = 1;
    p.b = 2;
    p.c = 3;
    p.d = 4;
    if (p.a + p.b + p.c + p.d != 10) return 2;
    return 1;
}
