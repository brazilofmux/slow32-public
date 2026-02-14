/* test6.c — switch/case, do-while, break, continue */
int putchar(int c);

void print_int(int n) {
    if (n < 0) {
        putchar(45);
        n = 0 - n;
    }
    if (n >= 10) {
        print_int(n / 10);
    }
    putchar(48 + n % 10);
}

void print_str(char *s) {
    while (*s) {
        putchar(*s);
        s = s + 1;
    }
}

void print_nl() {
    putchar(10);
}

char *color_name(int c) {
    switch (c) {
        case 0: return "red";
        case 1: return "green";
        case 2: return "blue";
        default: return "unknown";
    }
}

int main() {
    int i;

    /* Test switch */
    i = 0;
    while (i < 4) {
        print_int(i);
        putchar(61);
        print_str(color_name(i));
        putchar(32);
        i = i + 1;
    }
    print_nl();

    /* Test do-while */
    i = 1;
    do {
        print_int(i);
        putchar(32);
        i = i + i;
    } while (i < 32);
    print_nl();

    /* Test break in loop */
    i = 0;
    while (1) {
        if (i >= 5) break;
        print_int(i);
        putchar(32);
        i = i + 1;
    }
    print_nl();

    /* Test continue */
    i = 0;
    while (i < 10) {
        i = i + 1;
        if (i % 2 == 0) continue;
        print_int(i);
        putchar(32);
    }
    print_nl();

    return 0;
}
