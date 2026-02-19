#define ID(x) (x)
#define WRAP2(x, y) ((x) + (y))
#define APPLY2(fn, a, b) fn((a), (b))

int main(void) {
    int a = APPLY2(WRAP2, ID((10, 2)), ID((3 + 4)));
    int b = APPLY2(WRAP2, ID((1 + (2 * 3))), ID(((8 - 1), 5)));
    return (a == 9 && b == 12) ? 0 : 1;
}
