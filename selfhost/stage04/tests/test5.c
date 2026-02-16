/* test5.c — pointer arithmetic and array of structs */
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

struct point {
    int x;
    int y;
};

int main() {
    /* Test int pointer arithmetic: ptr + 1 should skip 4 bytes */
    int arr[4];
    arr[0] = 100;
    arr[1] = 200;
    arr[2] = 300;
    arr[3] = 400;

    int *p;
    p = arr;

    print_str("arr[0]=");
    print_int(*p);
    print_nl();

    p = p + 1;
    print_str("arr[1]=");
    print_int(*p);
    print_nl();

    p = p + 2;
    print_str("arr[3]=");
    print_int(*p);
    print_nl();

    /* Test sizeof */
    print_str("sizeof(int)=");
    print_int(sizeof(int));
    print_nl();

    print_str("sizeof(char)=");
    print_int(sizeof(char));
    print_nl();

    return 0;
}
