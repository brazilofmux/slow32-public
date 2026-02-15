static int get_shifted(const char *p, int i) {
    return (int)(p + 1)[i];
}

int main(void) {
    char a[5];
    a[0] = 3;
    a[1] = 6;
    a[2] = 9;
    a[3] = 12;
    a[4] = 15;
    return (get_shifted(a, 2) == 12) ? 0 : 1;
}
