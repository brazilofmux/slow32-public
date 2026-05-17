int main(void) {
    int a[3];
    int *p = a;
    p[0] = 4;
    p[1] = 5;
    p[2] = 6;
    return (a[0] + a[1] + a[2] == 15) ? 0 : 1;
}
