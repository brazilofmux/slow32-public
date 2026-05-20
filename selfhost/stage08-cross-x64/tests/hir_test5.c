/* Test globals, for loops, switch, bitwise ops */
int counter;

void inc(void) { counter = counter + 1; }

int bitcount(int x) {
    int n;
    n = 0;
    while (x) {
        n = n + (x & 1);
        x = x >> 1;
    }
    return n;
}

int classify(int x) {
    if (x < 0) return -1;
    if (x == 0) return 0;
    if (x < 10) return 1;
    if (x < 100) return 2;
    return 3;
}

int main() {
    int i;
    int sum;

    counter = 0;
    i = 0;
    while (i < 10) {
        inc();
        i = i + 1;
    }

    sum = counter;             /* 10 */
    sum = sum + bitcount(255); /* 10 + 8 = 18 */
    sum = sum + classify(-5);  /* 18 + (-1) = 17 */
    sum = sum + classify(0);   /* 17 + 0 = 17 */
    sum = sum + classify(7);   /* 17 + 1 = 18 */
    sum = sum + classify(42);  /* 18 + 2 = 20 */
    sum = sum + classify(999); /* 20 + 3 = 23 */

    return sum;
}
