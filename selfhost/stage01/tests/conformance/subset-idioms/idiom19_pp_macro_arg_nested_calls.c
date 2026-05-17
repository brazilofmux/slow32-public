#define ADD(a, b) ((a) + (b))
#define WRAP(x) (x)
#define TWICE(z) ADD((z), (z))

int main(void) {
    int v = TWICE(ADD(2, 3));
    int w = WRAP(ADD(4, ADD(1, 2)));
    return (v == 10 && w == 7) ? 0 : 1;
}
