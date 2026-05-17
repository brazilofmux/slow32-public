/* test4.c — Phase 2: structs, enums, typedef, struct pointers */
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

/* Struct definition */
struct point {
    int x;
    int y;
};

/* Print a point via pointer */
void print_point(struct point *p) {
    putchar(40);
    print_int(p->x);
    print_str(", ");
    print_int(p->y);
    putchar(41);
}

/* Enum definition */
enum color { RED, GREEN, BLUE };

/* Typedef */
typedef int myint;

int main() {
    /* Test struct fields */
    struct point p;
    p.x = 10;
    p.y = 20;
    print_str("p = ");
    print_point(&p);
    print_nl();

    /* Test struct pointer and -> */
    struct point *pp;
    pp = &p;
    pp->x = 30;
    print_str("pp->x=30: p = ");
    print_point(&p);
    print_nl();

    /* Test enum values */
    print_str("RED=");
    print_int(RED);
    print_str(" BLUE=");
    print_int(BLUE);
    print_nl();

    /* Test typedef */
    myint a;
    a = 42;
    print_str("myint a=");
    print_int(a);
    print_nl();

    /* Test sum of struct fields */
    print_str("x+y=");
    print_int(p.x + p.y);
    print_nl();

    return 0;
}
