static int get3(const unsigned char *p) {
    return (int)p[3];
}

int main(void) {
    unsigned char a[5];
    a[0] = 9;
    a[1] = 8;
    a[2] = 7;
    a[3] = 6;
    a[4] = 5;
    return (get3(a) == 6) ? 0 : 1;
}
