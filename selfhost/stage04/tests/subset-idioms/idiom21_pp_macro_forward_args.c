#define ADD(a, b) ((a) + (b))
#define APPLY2(f, x, y) f((x), (y))
#define CALL_ADD(x, y) APPLY2(ADD, (x), (y))

int main(void) {
    int v = CALL_ADD(5, 8);
    int w = CALL_ADD((2 + 3), (4 + 1));
    return (v == 13 && w == 10) ? 0 : 1;
}
