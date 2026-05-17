#define ID(x) (x)
#define ADD(a, b) ((a) + (b))

int main(void) {
    int a = ID((1, 2));
    int b = ADD((3, 4), 5);
    return (a == 2 && b == 9) ? 0 : 1;
}
