static int sum_mid(const char *p) {
    return (int)p[1] + (int)p[2];
}

int main(void) {
    char a[4];
    a[0] = 1;
    a[1] = 4;
    a[2] = 5;
    a[3] = 9;
    return (sum_mid(a) == 9) ? 0 : 1;
}
