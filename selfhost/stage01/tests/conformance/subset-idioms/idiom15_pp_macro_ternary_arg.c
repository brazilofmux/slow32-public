#define PICK(c, a, b) ((c) ? (a) : (b))

int main(void) {
    int x = PICK(1, 7, 9);
    int y = PICK(0, 7, 9);
    return (x == 7 && y == 9) ? 0 : 1;
}
