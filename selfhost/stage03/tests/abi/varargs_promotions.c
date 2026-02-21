typedef char *va_list;

int sum_u16_s16_after8(int a1, int a2, int a3, int a4,
                       int a5, int a6, int a7, int a8, ...) {
    va_list ap;
    int x;
    int y;
    va_start(ap, a8);
    x = va_arg(ap, int);
    y = va_arg(ap, int);
    va_end(ap);
    return x + y;
}

int main(void) {
    unsigned short u;
    short s;

    u = (unsigned short)65535;
    s = (short)-1;

    /* varargs arguments undergo default promotions to int */
    if (sum_u16_s16_after8(1, 2, 3, 4, 5, 6, 7, 8, u, s) != 65534) return 1;

    u = (unsigned short)1234;
    s = (short)-234;
    if (sum_u16_s16_after8(10, 20, 30, 40, 50, 60, 70, 80, u, s) != 1000) return 2;

    return 0;
}
