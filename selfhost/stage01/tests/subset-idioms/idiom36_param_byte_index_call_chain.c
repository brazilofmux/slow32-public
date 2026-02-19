static int add2(int a, int b) {
    return a + b;
}

static int fold3(const unsigned char *p, int i) {
    return add2((int)p[i], add2((int)p[i + 1], (int)p[i + 2]));
}

int main(void) {
    unsigned char data[8];
    int got;
    data[0] = 1;
    data[1] = 5;
    data[2] = 7;
    data[3] = 9;
    data[4] = 11;
    data[5] = 13;
    data[6] = 17;
    data[7] = 19;
    got = fold3(data, 2);
    return (got == 27) ? 0 : 1;
}
