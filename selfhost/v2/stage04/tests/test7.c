/* test7.c — preprocessor features */
int putchar(int c);

void print_int(int n) {
    if (n < 0) { putchar(45); n = 0 - n; }
    if (n >= 10) print_int(n / 10);
    putchar(48 + n % 10);
}

void print_nl() { putchar(10); }

/* Object-like macros */
#define TEN 10
#define TWENTY 20

/* Function-like macros */
#define ADD(a, b) ((a) + (b))
#define MUL(x, y) ((x) * (y))
#define SQUARE(n) MUL(n, n)

/* Conditional compilation */
#define FEATURE_A
#define HAS_MATH 1

int main() {
    /* Test object-like macros */
    print_int(TEN);
    putchar(32);

    /* Test function-like macros */
    print_int(ADD(3, 4));
    putchar(32);

    /* Test nested macro in function-like arg */
    print_int(ADD(TEN, 5));
    putchar(32);

    /* Test nested function-like macros */
    print_int(SQUARE(3));
    putchar(32);

    /* Test #ifdef */
#ifdef FEATURE_A
    print_int(1);
#else
    print_int(0);
#endif
    putchar(32);

    /* Test #ifndef */
#ifndef FEATURE_B
    print_int(1);
#else
    print_int(0);
#endif
    putchar(32);

    /* Test #if with defined() */
#if defined(FEATURE_A)
    print_int(1);
#else
    print_int(0);
#endif
    putchar(32);

    /* Test #if with !defined() */
#if !defined(FEATURE_B)
    print_int(1);
#else
    print_int(0);
#endif
    putchar(32);

    /* Test #if with integer expression */
#if HAS_MATH == 1
    print_int(1);
#else
    print_int(0);
#endif
    putchar(32);

    /* Test #if with && */
#if defined(FEATURE_A) && HAS_MATH
    print_int(1);
#else
    print_int(0);
#endif
    putchar(32);

    /* Test #undef */
#undef TEN
#define TEN 99
    print_int(TEN);
    putchar(32);

    /* Test #elif */
#define MODE 2
#if MODE == 1
    print_int(10);
#elif MODE == 2
    print_int(20);
#elif MODE == 3
    print_int(30);
#else
    print_int(40);
#endif

    print_nl();
    return 0;
}
