/* test3.c — Loops, arrays, pointers */
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

int main() {
    int arr[10];
    int i;
    int sum;

    /* Fill array */
    for (i = 0; i < 10; i = i + 1) {
        arr[i] = i * i;
    }

    /* Sum array */
    sum = 0;
    for (i = 0; i < 10; i = i + 1) {
        sum = sum + arr[i];
    }

    print_str("Sum of squares 0..9 = ");
    print_int(sum);
    putchar(10);

    /* Pointer walking */
    int *p;
    p = arr;
    print_str("First three: ");
    print_int(*p);
    putchar(32);
    p = p + 1;
    print_int(*p);
    putchar(32);
    p = p + 1;
    print_int(*p);
    putchar(10);

    return 0;
}
