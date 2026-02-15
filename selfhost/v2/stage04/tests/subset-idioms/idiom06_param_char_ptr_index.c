static int get2(const char *p) {
    return (int)p[2];
}

int main(void) {
    char a[4];
    a[0] = 1;
    a[1] = 2;
    a[2] = 3;
    a[3] = 4;
    return (get2(a) == 3) ? 0 : 1;
}
