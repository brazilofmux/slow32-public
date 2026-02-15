static void fill(int *a, int *b, int *c, int *d) {
    *a = 11;
    *b = 22;
    *c = 33;
    *d = 44;
}

int main(void) {
    int a = 0;
    int b = 0;
    int c = 0;
    int d = 0;
    fill(&a, &b, &c, &d);
    if (a != 11) return 1;
    if (b != 22) return 2;
    if (c != 33) return 3;
    if (d != 44) return 4;
    return 0;
}
