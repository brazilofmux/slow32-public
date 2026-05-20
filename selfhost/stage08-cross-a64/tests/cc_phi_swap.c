int main(void) {
    int a, b, i, t;
    a = 1;
    b = 2;
    i = 0;
    while (i < 5) {
        t = a;
        a = b;
        b = t;
        i = i + 1;
    }
    return a * 10 + b;
}
