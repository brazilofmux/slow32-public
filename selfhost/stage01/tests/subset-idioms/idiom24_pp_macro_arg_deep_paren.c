#define SUM3(a, b, c) ((a) + (b) + (c))

int main(void) {
    int v = SUM3(((1 + 2)), (((3 + 4))), ((5 + (6 - 1))));
    return (v == 20) ? 0 : 1;
}
