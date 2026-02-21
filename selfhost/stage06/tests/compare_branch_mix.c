int mix_step(int a, int b, int c) {
    int x;
    int y;
    x = (a + b) ^ (c << 1);
    y = (a - c) + (b >> 1);
    if (x > y) {
        return (x - y) + (a & 15);
    }
    if (x < y) {
        return (y - x) + (b | 7);
    }
    return x + y;
}

int main(void) {
    int i;
    int s;
    s = 0;
    for (i = 0; i < 64; i = i + 1) {
        s = s + mix_step(i, i + 3, i ^ 9);
    }
    return s;
}
