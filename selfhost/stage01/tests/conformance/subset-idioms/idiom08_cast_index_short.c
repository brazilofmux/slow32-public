static int get2(const short *p) {
    return (int)p[2];
}

int main(void) {
    short a[4];
    a[0] = 10;
    a[1] = 20;
    a[2] = 30;
    a[3] = 40;
    return (get2(a) == 30) ? 0 : 1;
}
