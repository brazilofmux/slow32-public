static int get_at(const char *p, int i) {
    return (int)p[i + 1];
}

int main(void) {
    char a[6];
    a[0] = 11;
    a[1] = 22;
    a[2] = 33;
    a[3] = 44;
    a[4] = 55;
    a[5] = 66;
    return (get_at(a, 2) == 44) ? 0 : 1;
}
