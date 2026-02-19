#define SECOND(x, y) (y)
#define PICK3(a, b, c) (c)

int main(void) {
    int a = SECOND((1 + 2), (3 + 4));
    int b = PICK3((10, 20), 30, 40);
    return (a == 7 && b == 40) ? 0 : 1;
}
