/* Diagnose struct layout: print member offsets */

int write(int fd, char *buf, int len);

static void print_int(int v) {
    char buf[12];
    int i;
    i = 11;
    buf[i] = '\n';
    if (v == 0) { i = i - 1; buf[i] = '0'; }
    while (v > 0) {
        i = i - 1;
        buf[i] = '0' + (v % 10);
        v = v / 10;
    }
    write(1, buf + i, 12 - i);
}

struct test {
    int a[32];
    int b;
    char *p;
    int c;
    int d;
    int e;
    int f;
    int arr[4];
};

int main(int argc, char **argv) {
    struct test t;
    char *base;
    char *mp;

    base = (char *)&t;

    write(1, "a: ", 3);
    print_int((int)((char *)&t.a - base));

    write(1, "b: ", 3);
    print_int((int)((char *)&t.b - base));

    write(1, "p: ", 3);
    print_int((int)((char *)&t.p - base));

    write(1, "c: ", 3);
    print_int((int)((char *)&t.c - base));

    write(1, "d: ", 3);
    print_int((int)((char *)&t.d - base));

    write(1, "arr: ", 5);
    print_int((int)((char *)&t.arr - base));

    return 0;
}
